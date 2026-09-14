#!/usr/bin/env bash
set -euo pipefail

SOURCE_OUTPUT="${SHARE_MIRROR_SOURCE:-DP-12}"
REGION="${SHARE_MIRROR_REGION:-0,0 1440x1280}"
PID_FILE="${XDG_RUNTIME_DIR:-/tmp}/share-mirror.pid"

stop_mirror() {
  if [[ -f "$PID_FILE" ]]; then
    kill "$(cat "$PID_FILE")" 2>/dev/null || true
    rm -f "$PID_FILE"
  fi
  pkill -f "wl-mirror.*${SOURCE_OUTPUT}" 2>/dev/null || true
}

start_mirror() {
  local target="${1:?missing mirror output name}"

  stop_mirror

  wl-mirror \
    --region "$REGION" \
    --fullscreen \
    --fullscreen-output "$target" \
    --scaling exact \
    --no-show-cursor \
    "$SOURCE_OUTPUT" &

  echo $! >"$PID_FILE"
}

case "${1:-}" in
  start)
    start_mirror "$2"
    ;;
  stop)
    stop_mirror
    ;;
  status)
    if [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
      echo "running (pid $(cat "$PID_FILE"))"
      exit 0
    fi
    echo "stopped"
    exit 1
    ;;
  *)
    echo "usage: $0 {start <output>|stop|status}" >&2
    exit 2
    ;;
esac