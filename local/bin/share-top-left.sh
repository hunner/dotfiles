#!/usr/bin/env bash

set -euo pipefail

if ! command -v xrandr >/dev/null 2>&1; then
  echo "Missing required command: xrandr" >&2
  exit 1
fi

exec xrandr \
  --output DisplayPort-3 \
  --mode 2560x1600 \
  --pos 0x0 \
  --scale-from 1440x1280
