#!/bin/sh

export PATH=/home/hunner/local/bin:$PATH

# added for nixos 26.05
export XDG_SESSION_TYPE=x11
dbus-update-activation-environment --systemd XDG_SESSION_TYPE DISPLAY

xsetroot -cursor_name right_ptr
redshift &
CM_DIR=~/.config/clipmenu clipmenud &
hsetroot -solid '#000000'
eval `ssh-agent -s`
battery.sh &
xset r rate 200 30

xset s on
xset s 1200
#xss-lock -- i3lock &
#xss-lock -- slock &
#xss-lock -- xlock -echokeys -usefirst &
#xscreensaver &

exec xmonad
