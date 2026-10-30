#!/bin/sh
# Repaint every AeroSpace workspace item in one batched sketchybar call.
#
# Run from AeroSpace's exec-on-workspace-change (set in the nix repo at
# hosts/slice/configuration.nix) and once from sketchybarrc at startup.
#
# Absolute paths throughout on purpose: AeroSpace runs from a launchd agent
# with no PATH set at all, so a bare `sketchybar` or `aerospace` is not found.
#
# Every workspace has an item on EVERY display -- items are named
# space.<display>.<workspace> -- because a sketchybar item carries one state but
# is drawn on all displays, so a single set could never highlight a different
# workspace per monitor. Each display's set is pinned with `display=` at
# creation; this script only flips the highlight.
#
# POSIX sh, no bashisms: macOS ships bash 3.2, which has no associative arrays.
SKETCHYBAR=/run/current-system/sw/bin/sketchybar

# Pick whichever aerospace CLI can actually reach the running server. AeroSpace
# keys its socket by version, so the nix CLI (0.21.3-Beta) cannot talk to a
# locally built fork (0.0.0-SNAPSHOT) and vice versa -- the mismatch fails
# silently and the bar just stops updating. Try each and use the one that
# answers.
AEROSPACE=
for candidate in \
  /run/current-system/sw/bin/aerospace \
  "$HOME/git/AeroSpace/.build/debug/aerospace"
do
  if [ -x "$candidate" ] && "$candidate" list-monitors >/dev/null 2>&1; then
    AEROSPACE=$candidate
    break
  fi
done
[ -n "$AEROSPACE" ] || exit 0

WORKSPACES="1 2 3 4 5 6 7 8 9 10"

# Map AeroSpace monitor ids onto sketchybar display indices.
#
# These are three different numbering schemes and only two of them agree.
# AeroSpace numbers monitors spatially, left to right. sketchybar's `display=`
# wants its own arrangement-id. AeroSpace can report an NSScreen index, which
# looks like the right bridge but is not: with three monitors here the NSScreen
# order was 3,1,2 while the arrangement-ids in spatial order were 2,1,3, so the
# bars for the leftmost and rightmost monitors were swapped.
#
# So ask sketchybar for its own displays, sort them by position to get the same
# spatial order AeroSpace uses, and pair them off by rank.
display_order=$("$SKETCHYBAR" --query displays 2>/dev/null | awk '
  /"arrangement-id"/ { gsub(/[^0-9]/, ""); id = $0 }
  /"x":/             { gsub(/[^0-9.-]/, ""); x = $0 }
  /"y":/             { if (id != "") { gsub(/[^0-9.-]/, ""); print x, $0, id; id = "" } }
' | sort -n -k1 -k2 | awk '{ print $3 }')

monitors=$("$AEROSPACE" list-monitors \
  --format '%{monitor-id}|%{monitor-appkit-nsscreen-screens-id}' 2>/dev/null)

set --
rank=0
for entry in $monitors; do
  mon=${entry%%|*}
  rank=$((rank + 1))
  # Nth monitor spatially gets the Nth arrangement-id. Fall back to the
  # NSScreen index if sketchybar told us nothing.
  disp=$(printf '%s\n' "$display_order" | sed -n "${rank}p")
  [ -n "$disp" ] || disp=${entry##*|}

  # Exactly one workspace is visible per monitor; that is the one to light up.
  visible=$("$AEROSPACE" list-workspaces --monitor "$mon" --visible \
    --format '%{workspace}' 2>/dev/null)

  for ws in $WORKSPACES; do
    if [ "$ws" = "$visible" ]; then draw=on; else draw=off; fi
    # display= and click_script are set here rather than at item creation
    # because both depend on the monitor-id to NSScreen-index mapping, and that
    # mapping changes when monitors are plugged in or rearranged.
    set -- "$@" --set "space.$disp.$ws" \
      display="$disp" \
      background.drawing="$draw" \
      click_script="$AEROSPACE eval 'focus-monitor $mon && summon-workspace $ws'"
  done
done

[ "$#" -gt 0 ] && exec "$SKETCHYBAR" "$@"
