#!/usr/bin/env bash

xrandr --output eDP-1 --auto
xrandr --output DP-2 --auto
xrandr --output DP-3 --auto
xrandr --output DP-2 --rotate normal
xrandr --output DP-3 --left-of eDP-1
xrandr --output DP-2 --left-of DP-3
xrandr --output DP-2 --primary
