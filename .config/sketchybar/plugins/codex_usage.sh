#!/bin/sh
# Repaint Codex's two usage bars.
#
# Source is the authenticated account/rateLimits/read RPC on `codex app-server`,
# which proxies GET https://chatgpt.com/backend-api/wham/usage. No model turn is
# started, so this costs quota nothing.
#
# What the two bars mean follows the account rather than a fixed pair of window
# lengths. The reply names each window only by its own length, and which
# lengths exist has already moved underneath this script once: a seat upgrade
# dropped `secondary` outright, leaving a single weekly window where the code
# insisted on finding a five hour one, and the widget sat frozen on numbers
# that looked fine for forty five minutes. So the windows present are
# discovered and ranked by duration:
#
#   bottom  the longest window the plan reports, the week on everything seen
#   top     the next shorter window when the plan has one, the five hour window
#           where it exists; otherwise the individual spend allowance, which is
#           real usage worth drawing and beats leaving half the widget blank
#
# Bottom stays the longer of the two because that is where the eye already
# looks for it. Since the mapping is now a property of the account rather than
# of this file, the breadcrumb names what each bar actually drew.
#
# Either value is worth drawing without the other. A plan change that costs one
# number should cost one bar, so a half readable reply still paints the half it
# can and says so; only a reply with nothing drawable in it fails as `shape`.
#
# Absolute paths throughout: sketchybar runs from a launchd agent with a minimal
# PATH, where only an absolute path resolves.
#
# The icon carries three states, and the whole shape of this script exists to
# tell the third one apart from the second:
#
#   white      a reading that is current
#   dim white  the drawn numbers have aged past STALE_AFTER, or something
#              transient is failing. Recoverable with no human involved.
#   red        nothing here comes back without a person. An expired credential
#              is one way in; a reply whose shape this script cannot read and a
#              codex binary that has gone missing are the others. It sat broken
#              for four hours once and frozen for another forty five, both
#              times because dim gray looks exactly like a passing blip.
#
# What sorts red from gray is recoverability, which cuts across the error
# taxonomy below rather than lining up with it. A timeout or an odd single
# reply heals itself and stays gray; anything that waits on a person goes red,
# even though the move it waits for differs between `codex login`, a reinstall
# and an afternoon spent editing this file. The breadcrumb's need= field says
# which one it is, because by then the icon alone has said all it can.
#
# Reading this costs an entire process spawn rather than one HTTP call, which is
# why the poll runs every five minutes (update_freq in sketchybarrc) and why a
# failure arms a backoff gate beside the cache. The gate here is about the cost
# of the attempt rather than about any quota: there is no evidence this endpoint
# rate-limits, but a spawn that just failed for a reason that persists will
# almost certainly fail again, and paying for that every five minutes for hours
# is waste with nothing to show for it.
#
# Two ladders, because the two kinds of failure recover on different timescales:
#
#   Soft failures (a timeout, codex missing, a response this script cannot read)
#   clear on their own, often within one poll. SOFT_FLOOR through SOFT_CEIL
#   stays narrow enough that the worst case is one skipped poll, so the gate
#   absorbs a burst of invocations without ever hiding a recovery.
#
#   Expired or absent auth recovers only when a human acts, so retrying it on
#   the poll cadence for hours accomplishes nothing. AUTH_FLOOR starts at three
#   poll intervals and widens to AUTH_CEIL. A ceiling that wide would normally
#   trade a slow recovery for the saved spawns, except that the recovery has a
#   cheap local tell: `codex login` rewrites ~/.codex/auth.json, so a gate armed
#   before that file was last written is known to be stale and opens at once.
#   That decouples the two, and is why the ceiling can afford to be an hour.
#
# The gate sits at the top and covers every invocation, whatever woke it. The
# five minute timer and system_woke are the callers we know about, and treating
# them as the only ones would be an assumption worth avoiding.
#
# Freshness is a property of the cached reading rather than of the last request.
# Through a soft failure the numbers already on screen are still exactly right,
# so the icon stays bright; only a cache that has gone STALE_AFTER without a
# success dims it. Red overrides that entirely, because a reading five seconds
# old says nothing about whether the thing that produced it still works.

SKETCHYBAR=/run/current-system/sw/bin/sketchybar
PYTHON=/usr/bin/python3
CODEX=/opt/homebrew/bin/codex

# Consulted for its mtime alone, never read. A login rewrites it, which is the
# only cheap local evidence this script has that a red state may have cleared.
AUTH_FILE="$HOME/.codex/auth.json"

# "<top percent> <bottom percent> <top label> <bottom label>", where a label of
# `none` marks a bar the last good reply had no number for.
CACHE="$HOME/.cache/sketchybar-codex-usage"
# "<epoch second the gate opens> <strikes> <class> <epoch second armed> <structural strikes>"
GATE="$CACHE.gate"
# "<icon state drawn last run> <what it is waiting on>", so the pulse fires on
# the edge only and so red keeps its reason through the gated runs that follow.
STATE="$CACHE.state"
STATUS="$CACHE.status"  # one line of "what happened last run", for debugging

ICON_OK=0xffffffff
ICON_STALE=0x66ffffff

# Apple's dark-appearance systemRed. The bar is 0x40000000 over a 30pt blur of
# whatever the desktop happens to be, so the backdrop is unpredictable and
# sometimes light. Pure 0xffff0000 turns muddy against a dark blur and
# disappears against a warm wallpaper; this one is lifted in luminance and
# pulled slightly toward orange, which is exactly what macos does to red for
# translucent dark material. It separates from both white states by luminance as
# well as by hue, so it still reads for anyone who sees red poorly.
ICON_ATTENTION=0xffff453a

# The peak of the single pulse that announces the change into red: the same hue
# mixed about two thirds toward white. Flaring to plain white would read as the
# icon briefly going healthy, which is the opposite of the message.
ICON_ATTENTION_FLARE=0xffffbeba

# A bar with no number behind it and a bar that genuinely reads zero both draw
# as an empty track, so the track itself carries the difference: nothing known
# fades it almost out, a real zero keeps it at full strength. It is a quiet
# distinction on something three pixels tall, and it is the whole of what this
# widget can say without a label, so the breadcrumb stays the authoritative
# answer. All this has to do is stop a missing value from passing as a healthy
# zero at a glance.
TRACK_KNOWN=0x33ffffff
TRACK_NONE=0x11ffffff

# Ticks run at 60/s, measured by sampling icon.color through an animation, so
# this is two 0.3s halves. One blink is enough to pull the eye. A bar that
# flashes forever becomes something you learn to ignore, and this state can sit
# for hours.
PULSE_CURVE=sin
PULSE_TICKS=18

# Dim once the cached reading has outlived several poll intervals. At a five
# minute poll, 15 minutes is three consecutive misses, which is firmly into
# "this number may be wrong".
STALE_AFTER=900

SOFT_FLOOR=60
SOFT_CEIL=300
AUTH_FLOOR=900
AUTH_CEIL=3600

# How many replies in a row this script has to fail to read before it stops
# calling that transient and turns red. Once the gate widens to its soft
# ceiling the attempts land five minutes apart, so three of them is about ten
# minutes: late enough that one strange reply from a mid-deploy backend passes
# unseen, early enough that a contract change is on screen before the cache has
# even aged into gray at fifteen. A missing binary skips the count entirely,
# since homebrew will not be putting it back by itself.
ATTENTION_STRIKES=3

/bin/mkdir -p "$(/usr/bin/dirname "$CACHE")"

now=$(/bin/date +%s)

prev_state=none
prev_need=none
if [ -r "$STATE" ]; then
  read -r prev_state prev_need < "$STATE"
  case "$prev_state" in
    ok | stale | attention) ;;
    # Written by the version that only ever went red for a credential. The
    # state means the same thing, so it is carried over rather than dropped.
    auth) prev_state=attention; prev_need=codex-login ;;
    *) prev_state=none ;;
  esac
  case "$prev_need" in
    codex-login | codex-install | plugin-edit) ;;
    *) prev_need=none ;;
  esac
fi

gate_until=0
strikes=0
gate_class=none
gate_armed=0
structural=0
if [ -r "$GATE" ]; then
  read -r gate_until strikes gate_class gate_armed structural < "$GATE"
  # Anything unreadable fails open, so a stray byte can never wedge the
  # indicator into permanent silence.
  case "$gate_until" in
    '' | *[!0-9]*) gate_until=0; strikes=0; gate_class=none; gate_armed=0 ;;
  esac
  case "$strikes" in
    '' | *[!0-9]*) strikes=0 ;;
  esac
  case "$gate_armed" in
    '' | *[!0-9]*) gate_armed=0 ;;
  esac
  # Absent in a gate file written before the escalation existed, which reads as
  # a clean slate and is the right place to start counting from.
  case "$structural" in
    '' | *[!0-9]*) structural=0 ;;
  esac
  case "$gate_class" in
    auth | soft) ;;
    *) gate_class=none ;;
  esac
fi

# A streak only describes the recent past. Once the gate has stood open longer
# than the widest window of its own class, whatever caused it has had every
# chance to clear and the next failure deserves to start counting from the floor
# again.
if [ "$gate_class" = "auth" ]; then
  decay=$AUTH_CEIL
else
  decay=$SOFT_CEIL
fi
if [ "$strikes" -gt 0 ] && [ "$now" -gt $((gate_until + decay)) ]; then
  strikes=0
  structural=0
  gate_class=none
fi

# The escape hatch described in the header. `codex login` writes a fresh token
# to auth.json, so a file newer than the moment the gate was armed means the
# evidence the gate rests on has been superseded. A routine token refresh also
# rewrites it, which is equally worth retrying for. stat on a missing file says
# nothing, and a logged-out machine has no auth.json at all, so 0 is the right
# reading there: it keeps the gate shut until the login that creates the file.
if [ "$gate_class" = "auth" ] && [ "$now" -lt "$gate_until" ]; then
  auth_mtime=$(/usr/bin/stat -f %m "$AUTH_FILE" 2>/dev/null)
  case "$auth_mtime" in
    '' | *[!0-9]*) auth_mtime=0 ;;
  esac
  if [ "$auth_mtime" -gt "$gate_armed" ]; then
    gate_until=0
    strikes=0
    gate_class=none
    /bin/rm -f "$GATE"
  fi
fi

if [ "$now" -lt "$gate_until" ]; then
  status="backoff"
else
  reading=$("$PYTHON" - "$CODEX" <<'PY' 2>/dev/null
import json
import math
import signal
import subprocess
import sys


def fail(reason):
    print("ERR " + reason)
    sys.exit(0)


class Timeout(Exception):
    """Raised out of the SIGALRM handler so one deadline covers the exchange."""


def on_alarm(signum, frame):
    raise Timeout


def is_auth(message):
    """Does this JSON-RPC error message describe a credential a human must fix?

    There is no HTTP status to read here: the failure arrives as a JSON-RPC
    error object whose `message` carries the upstream text. Two shapes were
    observed against codex-cli 0.162.1:

      logged out, no auth.json
        code -32600, "codex account authentication required to read rate limits"

      token present but rejected
        code -32603, "failed to fetch codex rate limits: GET
        https://chatgpt.com/backend-api/wham/usage failed: 401 Unauthorized;
        content-type=application/json; body={...}", where the body is
        {"code":"token_expired",...} for an aged token and
        {"detail":"Could not parse your authentication token..."} for a
        malformed one.

    The matching below stays deliberately narrow. "401" on its own turns up in
    error text about things other than this account, and "unauthorized" is a
    common enough word to catch an unrelated message, so the pair has to appear
    together. Everything that misses lands in the generic rpc class, which dims
    rather than alarms: a missed red costs a late diagnosis, while a false red
    sends someone to re-run a login that was fine all along.
    """
    m = message.lower()
    if "authentication required" in m:
        return True
    if "token_expired" in m:
        return True
    return "401" in m and "unauthorized" in m


def number(value):
    """Coerce one JSON field to a finite float, or to None if it is unusable.

    The payload mixes types for no reason visible from out here: usedPercent
    arrives as a number while the spend allowance's `used` and `limit` arrive
    as decimal strings. Accepting both in one place spares every caller from
    caring which it got today. bools are ints in python and would otherwise
    read as a perfectly plausible 0 or 1, so they are refused by name.
    """
    if isinstance(value, bool):
        return None
    if isinstance(value, str):
        try:
            value = float(value)
        except ValueError:
            return None
    if isinstance(value, (int, float)):
        return value if math.isfinite(value) else None
    return None


def window_label(minutes):
    """Name a window by its length in the largest unit that divides it evenly.

    300 reads as 5h and 10080 as 1w, which is how whoever reads the breadcrumb
    already thinks of them. Anything that divides into nothing stays in minutes
    rather than being rounded into a lie.
    """
    minutes = int(round(minutes))
    for size, suffix in ((10080, "w"), (1440, "d"), (60, "h")):
        if minutes >= size and minutes % size == 0:
            return "%d%s" % (minutes // size, suffix)
    return "%dm" % minutes


def collect_windows(limits):
    """Every rolling window in the reply, as (minutes, usedPercent), shortest
    first.

    Matched on the length each window reports rather than on the field it
    arrived in, because `primary` only means whichever window the plan treats
    as headline, and that has already moved from the five hour window to the
    week. Scanning the values instead of naming primary and secondary also
    picks up a third window for free if some plan grows one, and skips the
    objects that carry no window at all.
    """
    found = {}
    for window in limits.values():
        if not isinstance(window, dict):
            continue
        minutes = number(window.get("windowDurationMins"))
        used = number(window.get("usedPercent"))
        if minutes is None or used is None or minutes <= 0:
            continue
        found[int(round(minutes))] = used
    return sorted(found.items())


def spend_percent(limits):
    """How much of the individual spend allowance is consumed, or None.

    remainingPercent sits right beside this and is useless: it read 100 against
    an allowance that was 0.11 percent spent, so it is rounded to whole
    percents and would draw a bar that is confidently wrong at whichever end it
    lands on. `used` and `limit` carry the real resolution, as strings, so the
    division happens here.

    A share of an unlimited allowance means nothing, and neither does a share
    of a limit that is absent, zero or not a number. Each of those returns
    nothing to draw rather than a guess dressed as a measurement.
    """
    credits = limits.get("credits")
    if isinstance(credits, dict) and credits.get("unlimited") is True:
        return None
    individual = limits.get("individualLimit")
    if not isinstance(individual, dict):
        return None
    cap = number(individual.get("limit"))
    used = number(individual.get("used"))
    if cap is None or used is None or cap <= 0 or used < 0:
        return None
    return used / cap * 100.0


def render(bar):
    """One bar as (label, whole percent), or the empty drawing.

    sketchybar wants an integer, and a bar with nothing behind it still has to
    be handed some number, so it gets a zero that the label disowns.
    """
    if bar is None:
        return ("none", 0)
    return (bar[0], max(0, min(100, int(bar[1]))))


# One deadline for the whole exchange rather than one per read, because codex
# can stall at the spawn, the handshake, or the upstream fetch, and only the
# total matters. 25s leaves the five minute poll plenty of room, so two runs can
# never overlap.
signal.signal(signal.SIGALRM, on_alarm)
signal.alarm(25)

process = None
try:
    try:
        process = subprocess.Popen(
            [sys.argv[1], "app-server", "--listen", "stdio://"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL, text=True,
        )
    except OSError:
        # Missing, non-executable, or the wrong architecture: homebrew moved
        # underneath this path, or an upgrade is mid-flight.
        fail("no-binary")

    def send(message):
        process.stdin.write(json.dumps(message) + "\n")
        process.stdin.flush()

    def receive(request_id):
        for line in process.stdout:
            try:
                message = json.loads(line)
            except ValueError:
                # A non-JSON line on stdout is log noise from some future
                # version. Skipping it costs nothing; treating it as fatal would
                # throw away a run that is about to succeed.
                continue
            if message.get("id") == request_id:
                return message
        return None

    try:
        send({"id": 1, "method": "initialize", "params": {
            "clientInfo": {"name": "sketchybar_usage", "version": "1.0"}
        }})
        handshake = receive(1)
        if handshake is None or "error" in handshake:
            # Launched, then died or refused the handshake. A version whose
            # protocol moved looks like this too.
            fail("no-handshake")
        send({"method": "initialized"})
        send({"id": 2, "method": "account/rateLimits/read"})
    except OSError:
        # The pipe closed mid-write, which means the process is already gone.
        fail("no-handshake")

    reply = receive(2)
    if reply is None:
        fail("no-response")

    if "error" in reply:
        message = str((reply.get("error") or {}).get("message") or "")
        fail("auth" if is_auth(message) else "rpc")

    data = reply.get("result")
    if not isinstance(data, dict):
        fail("shape")

    limits = (data.get("rateLimitsByLimitId") or {}).get("codex")
    if limits is None:
        limits = data.get("rateLimits") or {}
    if not isinstance(limits, dict):
        fail("shape")

    # Rank by duration rather than by name, then hand the longest window to the
    # bottom bar and whatever sits one step shorter to the top. A plan with
    # only one window leaves the top bar to the spend allowance, which is the
    # one other number here that measures something being used up.
    windows = collect_windows(limits)
    bottom = (window_label(windows[-1][0]), windows[-1][1]) if windows else None
    if len(windows) >= 2:
        top = (window_label(windows[-2][0]), windows[-2][1])
    else:
        spend = spend_percent(limits)
        top = ("spend", spend) if spend is not None else None

    if top is None and bottom is None:
        # Parsed, and then said nothing this script can draw. That is the
        # signature of an API change rather than of an outage, and with the
        # per-bar fallbacks above it is the only case left that deserves to
        # fail the whole read.
        fail("shape")

    # The labels travel with the numbers so the breadcrumb and the cache can
    # both say which quota each bar is standing for on this account.
    top_label, top_percent = render(top)
    bottom_label, bottom_percent = render(bottom)
    print("OK %d %d %s %s" % (top_percent, bottom_percent, top_label, bottom_label))
except Timeout:
    fail("timeout")
except Exception:
    # A last resort so this script always says one word about itself. Anything
    # landing here is a bug in the lines above rather than a condition they knew
    # how to name.
    fail("internal")
finally:
    signal.alarm(0)
    if process is not None:
        process.terminate()
        try:
            process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
PY
  )

  set -- $reading
  case "$1" in
    OK)
      # Only a success writes the cache, which is what lets its mtime stand in
      # for the age of the numbers on screen. One success also clears the gate
      # outright: whatever it was guarding against is demonstrably over.
      printf '%s %s %s %s\n' "$2" "$3" "$4" "$5" > "$CACHE"
      /bin/rm -f "$GATE"
      gate_until=0
      strikes=0
      structural=0
      gate_class=none
      # A reply that yielded one bar instead of two is still a success: the
      # endpoint answered, the credential works, and half a widget beats a
      # frozen one. It earns its own word only so the breadcrumb says out loud
      # that a bar is missing on purpose, since nothing on screen will.
      if [ "$4" = "none" ] || [ "$5" = "none" ]; then
        status="partial"
      else
        status="ok"
      fi
      ;;
    ERR)
      status=$2
      if [ "$status" = "auth" ]; then
        gate_class=auth
        floor=$AUTH_FLOOR
        ceil=$AUTH_CEIL
      else
        gate_class=soft
        floor=$SOFT_FLOOR
        ceil=$SOFT_CEIL
      fi
      strikes=$((strikes + 1))
      # A second count, for the failures that only a person can end. It is
      # cleared by a success and by nothing else: a timeout landing in the
      # middle of a run of unreadable replies is evidence about the network and
      # says nothing at all about whether the contract came back, so it neither
      # advances the count nor forgives it.
      [ "$status" = "shape" ] && structural=$((structural + 1))
      window=$floor
      n=1
      while [ "$n" -lt "$strikes" ] && [ "$window" -lt "$ceil" ]; do
        window=$((window * 2))
        n=$((n + 1))
      done
      [ "$window" -gt "$ceil" ] && window=$ceil
      gate_until=$((now + window))
      printf '%s %s %s %s %s\n' "$gate_until" "$strikes" "$gate_class" "$now" "$structural" > "$GATE"
      ;;
    *)
      # Empty or unparseable output means python itself never got to speak, e.g.
      # killed, missing, or crashed before the first print. Deliberately left
      # ungated, because a gate here would hide the one failure mode that is
      # cheap to retry and that no evidence explains.
      status="no-output"
      ;;
  esac
fi

# The bars always draw the last good reading, so a failure holds the previous
# numbers rather than dropping to zero, which would read as "no usage" instead
# of "no answer".
#
# The item names are codex.session and codex.week from the days when those were
# the only two windows going. They are the top and bottom bar now, and what
# each one holds comes from the cache alongside the number.
if [ -r "$CACHE" ]; then
  read -r top bottom top_kind bottom_kind < "$CACHE"
  # A cache written before the bars could vary carries two bare numbers. They
  # are still the last good reading and still worth drawing; the mapping just
  # goes unnamed until the next success rewrites the file.
  [ -n "$top_kind" ] || top_kind='?'
  [ -n "$bottom_kind" ] || bottom_kind='?'
  # `now` is sampled before the request, so a success that writes the cache a
  # moment later can date it after `now`. Clamp so the status line reports a
  # just-written reading as the zero seconds old that it is.
  age=$((now - $(/usr/bin/stat -f %m "$CACHE")))
  [ "$age" -lt 0 ] && age=0
else
  # Nothing has ever landed. Two faded empty tracks under a dim icon is the
  # honest drawing of "nothing is known yet".
  top=0
  bottom=0
  top_kind=none
  bottom_kind=none
  age=$STALE_AFTER
fi

# Only a verdict changes what is believed about this pipe. A success of either
# kind says it works, while a rejected credential, a vanished binary and a run
# of replies nobody here can read each name a person who has to act. A timeout,
# a dead pipe or a gated run say nothing either way, so they inherit what was
# believed last run. That stickiness is what holds the icon red through the
# backoff windows that follow, which is most of the time any of these last.
case "$status" in
  ok | partial) need=none ;;
  auth) need=codex-login ;;
  # A path that resolved to a binary yesterday and resolves to nothing today
  # got there through an uninstall or a half finished upgrade. Spending a
  # ladder to confirm that would only delay the reinstall.
  no-binary) need=codex-install ;;
  # A single unreadable reply is a bad afternoon at the backend; three in a row
  # is the contract having moved, and the only thing that fixes that is someone
  # opening this file.
  shape)
    if [ "$structural" -ge "$ATTENTION_STRIKES" ]; then
      need=plugin-edit
    else
      need=$prev_need
    fi
    ;;
  *) need=$prev_need ;;
esac

if [ "$need" != "none" ]; then
  state=attention
  icon_color=$ICON_ATTENTION
elif [ "$age" -lt "$STALE_AFTER" ]; then
  state=ok
  icon_color=$ICON_OK
else
  state=stale
  icon_color=$ICON_STALE
fi
printf '%s %s\n' "$state" "$need" > "$STATE"

# A breadcrumb for whoever debugs this next, overwritten every run: what the
# last attempt did, how old the drawn numbers are, how long the gate holds the
# next spawn back and on what grounds, how deep each streak has gone, which of
# the three states the icon ended in, what it is waiting on, and which quota
# each bar ended up standing for. Under launchd stderr goes nowhere, so this
# file is the only place any of the above becomes visible to a human, and it is
# where a red icon gets explained now that red has more than one cause.
if [ "$gate_until" -gt "$now" ]; then
  gate_note="closed-for-$((gate_until - now))s/$gate_class"
else
  gate_note="open"
fi
printf '%s status=%s age=%ss gate=%s strikes=%s/%s state=%s need=%s top=%s/%s%% bottom=%s/%s%%\n' \
  "$(/bin/date -r "$now" '+%Y-%m-%dT%H:%M:%S')" "$status" "$age" "$gate_note" \
  "$strikes" "$structural" "$state" "$need" \
  "$top_kind" "$top" "$bottom_kind" "$bottom" > "$STATUS"

# One pulse on the way into red and never again while it lasts, which is what
# the persisted previous state buys. The edge is the state rather than the
# reason behind it, so a red that changes its mind about which human it needs
# holds steady instead of flashing twice for one unbroken condition.
# sketchybar runs chained --animate stages back to back within a single
# invocation, so this is red, flare, red in one command with no sleep and no
# second process.
set -- --set codex.icon icon.color="$icon_color"
if [ "$state" = "attention" ] && [ "$prev_state" != "attention" ]; then
  set -- "$@" \
    --animate "$PULSE_CURVE" "$PULSE_TICKS" --set codex.icon icon.color="$ICON_ATTENTION_FLARE" \
    --animate "$PULSE_CURVE" "$PULSE_TICKS" --set codex.icon icon.color="$ICON_ATTENTION"
fi

# The track colour goes out on every run rather than only when it changes,
# since the one thing worse than a quiet distinction is a stale one.
[ "$top_kind" = "none" ] && top_track=$TRACK_NONE || top_track=$TRACK_KNOWN
[ "$bottom_kind" = "none" ] && bottom_track=$TRACK_NONE || bottom_track=$TRACK_KNOWN

exec "$SKETCHYBAR" "$@" \
  --set codex.session slider.percentage="$top" slider.background.color="$top_track" \
  --set codex.week slider.percentage="$bottom" slider.background.color="$bottom_track"
