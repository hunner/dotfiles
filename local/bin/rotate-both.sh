#!/usr/bin/env bash

xrandr --output eDP --auto --scale 1x1
xrandr --output DisplayPort-1 --auto --scale 1x1
xrandr --output DisplayPort-2 --auto --scale 1x1
xrandr --output DisplayPort-1 --rotate right
xrandr --output DisplayPort-2 --rotate left
xrandr --output DisplayPort-1 --left-of eDP
xrandr --output DisplayPort-2 --left-of DisplayPort-1
xrandr --output DisplayPort-2 --primary
