export PATH=/home/hunner/local/bin:$PATH

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?* ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

#export GDK_DPI_SCALE=0.5
systemctl --user import-environment DISPLAY
#setxkbmap us dvorak ctrl:nocaps
# remap powerkey to insert
#xmodmap -e "keycode 124 = NoSymbol Insert"
#xmodmap -e "keycode 62 = Scroll_Lock Scroll_Lock"
#xmodmap -e "keycode 37 = Control_R Control_R"
xinput set-prop 'Apple Inc. Magic Trackpad 2' 'libinput Tapping Enabled' 1
xsetroot -cursor_name right_ptr
redshift &
CM_DIR=~/.config/clipmenu clipmenud &
#picom -b
hsetroot -solid '#000000'
#xrdb -merge ~/.Xresources
#feh --bg-fill ~/Dropbox/Photos/Adventurous\ Kate.jpg
eval `ssh-agent -s`
xrandr  --dpi 100
battery.sh &
xset r rate 200 30

xset s on
xset s 1200
#xss-lock -- i3lock &
#xss-lock -- slock &
#xss-lock -- xlock -echokeys -usefirst &
#xscreensaver &


exec xmonad
