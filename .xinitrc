export PATH=/home/hunner/local/bin:$PATH

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?* ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

setxkbmap us dvorak ctrl:nocaps
# remap powerkey to insert
xmodmap -e "keycode 124 = NoSymbol Insert"
xmodmap -e "keycode 62 = Scroll_Lock Scroll_Lock"
xmodmap -e "keycode 37 = Control_R Control_R"
xset r rate 220 32
feh --bg-fill ~/Dropbox/Photos/Adventurous\ Kate.jpg
eval `ssh-agent -s`
battery.sh &

exec xmonad
