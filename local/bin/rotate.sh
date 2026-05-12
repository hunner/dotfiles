#!/usr/bin/env bash

xrandr --output eDP --auto
xrandr --output DisplayPort-1 --auto
xrandr --output DisplayPort-2 --auto
xrandr --output DisplayPort-1 --rotate normal
xrandr --output DisplayPort-2 --rotate right
xrandr --output DisplayPort-1 --left-of eDP
xrandr --output DisplayPort-2 --left-of DisplayPort-1
xrandr --output DisplayPort-2 --primary
