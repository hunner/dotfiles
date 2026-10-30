#!/bin/sh
# Repaint the three Claude subscription usage bars.
#
# Source is https://api.anthropic.com/api/oauth/usage, authenticated with the
# OAuth access token Claude Code stores in the login keychain under the service
# name "Claude Code-credentials". The response carries a limits[] array with one
# entry per window; the three drawn here are keyed by `kind`:
#
#   session       the rolling five hour window
#   weekly_all    the weekly allowance across all models
#   weekly_scoped the weekly allowance scoped to a single model, which is Fable
#                 on this account (limits[].scope.model.display_name)
#
# Absolute paths throughout: sketchybar runs from a launchd agent with a minimal
# PATH, where only an absolute path resolves.
#
# The endpoint rate-limits, and the shape of that limit drives everything below.
# It behaves like a token bucket with real burst depth and a slow refill rather
# than as a fixed minimum spacing between calls. Measured passively over fifteen
# minutes of this plugin polling on a 60s timer: three consecutive 60s polls all
# succeeded, and four requests once landed inside 64 seconds, yet the same
# window held two blackouts near four minutes long where every scheduled poll
# was rejected. Success on that cadence ran around 40%.
#
# Rejections are HTTP 429 carrying {"error":{"type":"rate_limit_error",...}},
# turned away at the Cloudflare edge in about 0.09s. The reply says nothing
# useful about the limit: it carries no anthropic-ratelimit-* headers, and its
# `Retry-After: 0` is false. The budget is per account rather than per process,
# so every Claude Code session on this machine drinks from the same bucket, and
# the account's own usage climbing while this was measured confirms there were
# several of them.
#
# Three consequences:
#
#   The timed poll runs every five minutes (update_freq in sketchybarrc). That
#   lowers the pressure this indicator puts on a shared bucket, though taken
#   alone it would only move the dry spells around rather than remove them. The
#   windows drawn here are five hours and a week wide, so five minute
#   granularity costs nothing anyone can see.
#
#   A 429 arms a backoff gate persisted beside the cache, and the gate widens
#   with each consecutive rejection: BACKOFF_FLOOR, doubling toward
#   BACKOFF_CEIL, collapsing to nothing on the first success. A bucket that
#   bursts and then drains for minutes is served badly by any single constant.
#   The floor stays short enough to catch a refill soon after it happens, and
#   the ceiling is wide enough to ride out a four minute blackout rather than
#   feed requests into it. Every request spent on a certain 429 keeps the bucket
#   drained for the run that would have succeeded.
#
#   The gate sits at the top and covers every invocation, whatever woke it. The
#   five minute timer and system_woke are the callers we know about, and
#   treating them as the only ones would be an assumption worth avoiding.
#
# Freshness is a property of the cached reading rather than of the last request,
# and with blackouts running to four minutes that distinction carries most of
# the value here: through a blackout the numbers already on screen are still
# exactly right, so the icon stays bright. Only a cache that has gone
# STALE_AFTER without a success dims it.
#
# That leaves the icon with three states:
#
#   white      a reading that is current
#   dim white  the drawn numbers have aged past STALE_AFTER, or something
#              transient is failing. Recoverable with no human involved.
#   red        no usable credential. Everything above is about outages that end
#              by themselves; this one ends when a person does something.
#
# Red overrides freshness rather than sharing a scale with it, because a reading
# five seconds old says nothing about whether the credential behind it still
# works. The distinction earns its own colour from experience: the sibling codex
# indicator sat dim gray for four hours on an expired token, which was true and
# useless, because dim gray is also what a passing blip looks like.

SKETCHYBAR=/run/current-system/sw/bin/sketchybar
PYTHON=/usr/bin/python3

CACHE="$HOME/.cache/sketchybar-claude-usage"
GATE="$CACHE.gate"      # "<epoch second the gate opens> <consecutive 429s>"
STATE="$CACHE.state"    # the icon state drawn last run, so the pulse fires on the edge only
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
ICON_AUTH=0xffff453a

# The peak of the single pulse that announces the change into red: the same hue
# mixed about two thirds toward white. Flaring to plain white would read as the
# icon briefly going healthy, which is the opposite of the message.
ICON_AUTH_FLARE=0xffffbeba

# Ticks run at 60/s, measured by sampling icon.color through an animation, so
# this is two 0.3s halves. One blink is enough to pull the eye. A bar that
# flashes forever becomes something you learn to ignore, and this state can sit
# for hours.
PULSE_CURVE=sin
PULSE_TICKS=18

# Dim once the cached reading has outlived several poll intervals. At a five
# minute poll, 15 minutes is three consecutive misses: comfortably past the
# four minute blackouts and past a token refresh, and firmly into "this number
# may be wrong".
STALE_AFTER=900

# The gate after one 429, and the widest it ever grows. The floor is short
# enough to exploit a bucket that refills between polls; the ceiling matches the
# poll interval, so even a long drought costs about one skipped poll rather than
# an open-ended silence.
BACKOFF_FLOOR=60
BACKOFF_CEIL=300

/bin/mkdir -p "$(/usr/bin/dirname "$CACHE")"

now=$(/bin/date +%s)

prev_state=none
if [ -r "$STATE" ]; then
  read -r prev_state < "$STATE"
  case "$prev_state" in
    ok | stale | auth) ;;
    *) prev_state=none ;;
  esac
fi

gate_until=0
strikes=0
if [ -r "$GATE" ]; then
  read -r gate_until strikes < "$GATE"
  # Anything unreadable fails open, so a stray byte can never wedge the
  # indicator into permanent silence.
  case "$gate_until" in
    '' | *[!0-9]*) gate_until=0; strikes=0 ;;
  esac
  case "$strikes" in
    '' | *[!0-9]*) strikes=0 ;;
  esac
fi

# A streak only describes the recent past. Once the gate has stood open longer
# than the widest window, the bucket has had time to refill and the next
# rejection deserves to start counting again from the floor.
if [ "$strikes" -gt 0 ] && [ "$now" -gt $((gate_until + BACKOFF_CEIL)) ]; then
  strikes=0
fi

if [ "$now" -lt "$gate_until" ]; then
  status="backoff"
else
  reading=$("$PYTHON" - <<'PY' 2>/dev/null
import json, subprocess, sys, urllib.error, urllib.request

def fail(reason):
    print("ERR " + reason)
    sys.exit(0)

try:
    raw = subprocess.run(
        ["/usr/bin/security", "find-generic-password",
         "-s", "Claude Code-credentials", "-w"],
        capture_output=True, text=True, timeout=10,
    )
    token = json.loads(raw.stdout)["claudeAiOauth"]["accessToken"]
except Exception:
    # No credential to send at all: the keychain item is gone, or holds
    # something this script cannot read. Same human remedy as a 401, so the
    # shell treats it the same way. A keychain that is merely locked would land
    # here too and paint red for one poll, which is a cheap price for catching
    # the logged-out case that would otherwise sit dim forever.
    fail("no-token")

req = urllib.request.Request(
    "https://api.anthropic.com/api/oauth/usage",
    headers={
        "Authorization": "Bearer " + token,
        "anthropic-beta": "oauth-2025-04-20",
    },
)
try:
    with urllib.request.urlopen(req, timeout=15) as resp:
        body = resp.read()
except urllib.error.HTTPError as e:
    # Each outcome keeps its own name so a dim icon can be traced to a cause
    # from the status file alone. 429 is the rate limiter, and the shell widens
    # its backoff gate on exactly this word. 401 means the stored access token
    # is being refused: a running Claude Code session refreshes it in passing,
    # but nothing here can, so on a machine where no session happens to run it
    # stays refused indefinitely. That is why the shell paints this one red
    # rather than dim. Any other code reports itself rather than hiding inside a
    # generic bucket.
    if e.code == 429:
        fail("rate-limit")
    if e.code == 401:
        fail("unauthorized")
    fail("http-%d" % e.code)
except Exception:
    # DNS, TLS, refused connection, timeout: the network rather than the API,
    # and a different problem from anything the API itself can say.
    fail("transport")

try:
    data = json.loads(body)
except Exception:
    # Valid HTTP carrying something other than JSON, e.g. a captive portal or
    # an edge error page served with a 200.
    fail("malformed")

# Key off `kind` rather than array position: the array omits windows that do not
# apply to the account, so indices shift between plans and over time.
by_kind = {l.get("kind"): l for l in data.get("limits") or [] if isinstance(l, dict)}

def pct(kind):
    entry = by_kind.get(kind)
    if not entry:
        return None
    value = entry.get("percent")
    return max(0, min(100, int(value))) if isinstance(value, (int, float)) else None

session, week, scoped = pct("session"), pct("weekly_all"), pct("weekly_scoped")
if session is None or week is None:
    # JSON that parsed but says something this script cannot read, which is the
    # signature of an API change rather than of an outage.
    fail("shape")

# weekly_scoped disappears when no model-scoped limit is in force. Treat that as
# zero rather than an error so the other two bars keep updating.
print("OK %d %d %d" % (session, week, scoped if scoped is not None else 0))
PY
  )

  set -- $reading
  case "$1" in
    OK)
      # Only a success writes the cache, which is what lets its mtime stand in
      # for the age of the numbers on screen. One success also clears the whole
      # streak: the bucket just proved it has tokens.
      printf '%s %s %s\n' "$2" "$3" "$4" > "$CACHE"
      /bin/rm -f "$GATE"
      gate_until=0
      strikes=0
      status="ok"
      ;;
    ERR)
      status=$2
      # Only a 429 gates. A transport error or a 401 costs the bucket nothing,
      # so holding the next attempt back would buy nothing and would delay the
      # recovery it is waiting for.
      if [ "$status" = "rate-limit" ]; then
        strikes=$((strikes + 1))
        window=$BACKOFF_FLOOR
        n=1
        while [ "$n" -lt "$strikes" ] && [ "$window" -lt "$BACKOFF_CEIL" ]; do
          window=$((window * 2))
          n=$((n + 1))
        done
        [ "$window" -gt "$BACKOFF_CEIL" ] && window=$BACKOFF_CEIL
        gate_until=$((now + window))
        printf '%s %s\n' "$gate_until" "$strikes" > "$GATE"
      fi
      ;;
    *)
      # Empty or unparseable output means python itself never got to speak, e.g.
      # killed, missing, or crashed before the first print.
      status="no-output"
      ;;
  esac
fi

# The bars always draw the last good reading, so a failure holds the previous
# numbers rather than dropping to zero, which would read as "no usage" instead
# of "no answer".
if [ -r "$CACHE" ]; then
  read -r session week scoped < "$CACHE"
  # `now` is sampled before the request, so a success that writes the cache a
  # moment later can date it after `now`. Clamp so the status line reports a
  # just-written reading as the zero seconds old that it is.
  age=$((now - $(/usr/bin/stat -f %m "$CACHE")))
  [ "$age" -lt 0 ] && age=0
else
  # Nothing has ever landed. Zeroed bars under a dim icon is the honest drawing
  # of "nothing is known yet".
  session=0
  week=0
  scoped=0
  age=$STALE_AFTER
fi

# Only a verdict changes what is believed about the credential. A success says
# it works and a 401 says it does not; a rate limit, a transport error or a
# gated run say nothing either way, so they inherit. Without that stickiness a
# single 429 landing on top of an expired token would quietly repaint the icon
# dim and lose the one signal worth having.
case "$status" in
  ok) auth_bad=0 ;;
  unauthorized | no-token) auth_bad=1 ;;
  *) [ "$prev_state" = "auth" ] && auth_bad=1 || auth_bad=0 ;;
esac

if [ "$auth_bad" -eq 1 ]; then
  state=auth
  icon_color=$ICON_AUTH
elif [ "$age" -lt "$STALE_AFTER" ]; then
  state=ok
  icon_color=$ICON_OK
else
  state=stale
  icon_color=$ICON_STALE
fi
printf '%s\n' "$state" > "$STATE"

# A breadcrumb for whoever debugs this next, overwritten every run: what the
# last attempt did, how old the drawn numbers are, how long the gate still holds
# the next request back, how deep the rejection streak has gone, and which of
# the three states the icon ended in. Under launchd stderr goes nowhere, so this
# file is the only place the taxonomy above becomes visible to a human.
if [ "$gate_until" -gt "$now" ]; then
  gate_note="closed-for-$((gate_until - now))s"
else
  gate_note="open"
fi
printf '%s status=%s age=%ss gate=%s strikes=%s state=%s\n' \
  "$(/bin/date -r "$now" '+%Y-%m-%dT%H:%M:%S')" "$status" "$age" "$gate_note" "$strikes" "$state" > "$STATUS"

# One pulse on the way into red and never again while it lasts, which is what
# the persisted previous state buys. sketchybar runs chained --animate stages
# back to back within a single invocation, so this is red, flare, red in one
# command with no sleep and no second process.
set -- --set claude.icon icon.color="$icon_color"
if [ "$state" = "auth" ] && [ "$prev_state" != "auth" ]; then
  set -- "$@" \
    --animate "$PULSE_CURVE" "$PULSE_TICKS" --set claude.icon icon.color="$ICON_AUTH_FLARE" \
    --animate "$PULSE_CURVE" "$PULSE_TICKS" --set claude.icon icon.color="$ICON_AUTH"
fi

exec "$SKETCHYBAR" "$@" \
  --set claude.session slider.percentage="$session" \
  --set claude.week slider.percentage="$week" \
  --set claude.fable slider.percentage="$scoped"
