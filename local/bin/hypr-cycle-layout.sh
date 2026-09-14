#!/usr/bin/env bash
set -euo pipefail

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/hypr"
mkdir -p "$cache_dir"

workspace_id="$(hyprctl activeworkspace -j | jq -r '.id')"
state_file="$cache_dir/layout-cycle-${workspace_id}"

fullscreen="$(hyprctl activewindow -j | jq -r '.fullscreen')"
if [[ "$fullscreen" != "0" ]]; then
  current="full"
elif [[ -f "$state_file" ]]; then
  current="$(<"$state_file")"
else
  current="vertical"
fi

case "$current" in
  vertical)
    next="horizontal"
    hyprctl dispatch 'hl.dsp.window.fullscreen({ mode = "maximized", action = "unset" })' >/dev/null
    hyprctl dispatch 'hl.dsp.layout("orientationtop")' >/dev/null
    ;;
  horizontal)
    next="full"
    hyprctl dispatch 'hl.dsp.window.fullscreen({ mode = "maximized", action = "set" })' >/dev/null
    ;;
  *)
    next="vertical"
    hyprctl dispatch 'hl.dsp.window.fullscreen({ mode = "maximized", action = "unset" })' >/dev/null
    hyprctl dispatch 'hl.dsp.layout("orientationleft")' >/dev/null
    ;;
esac

printf '%s\n' "$next" >"$state_file"