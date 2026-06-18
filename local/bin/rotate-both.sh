#!/usr/bin/env bash

set -euo pipefail

exec xrandr \
  --output DisplayPort-2 --primary --mode 2560x1440 --rotate left   --pos 0x0    --scale 1x1 \
  --output DisplayPort-1           --mode 2560x1440 --rotate right  --pos 1440x0 --scale 1x1 \
  --output eDP                     --mode 2560x1600 --rotate normal --pos 2880x0 --scale 1x1
