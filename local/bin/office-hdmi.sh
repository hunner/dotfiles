#!/usr/bin/env bash

xrandr --output eDP-1-1 --auto
xrandr --output HDMI-1-1 --auto
xrandr --output HDMI-1-1 --left-of eDP-1-1
xrandr --output HDMI-1-1 --primary
