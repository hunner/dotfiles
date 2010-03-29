export PATH=/home/hunner/.gems/bin:/home/hunner/.cabal/bin:/home/hunner/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/bin:/opt/java/bin:/opt/java/jre/bin:/usr/share/java/apache-ant/bin:/opt/java/bin:/opt/java/jre/bin:/usr/bin/perlbin/site:/usr/bin/perlbin/vendor:/usr/bin/perlbin/core:/opt/qt/bin
xkbcomp -w0 ~/keymaps/xkb/hunner.xkb $DISPLAY
#setxkbmap fr bepo "ctrl:swapcaps"
xsetroot -cursor_name left_ptr
#xkbcomp ~/keymaps/halfdvorak.xkb $DISPLAY
#xmodmap ~/keymaps/nu_x61.pke
#xmodmap ~/keymaps/colemak.pke
#habak -ms ~/Pics/wp/current
nitrogen --restore
xcompmgr -F &
#redshift -l 45.4:-122.6 &

#uim
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
export XMODIFIERS=@im='uim'
uim-xim &

#scim
#XMODIFIERS="@im=SCIM" ; export XMODIFIERS
#GTK_IM_MODULE="scim" ; export GTK_IM_MODULE
#QT_IM_MODULE="scim" ; export QT_IM_MODULE
#scim -d
#XIM_PROGRAM="scim -d" ; export XIM_PROGRAM

/home/hunner/local/bin/urxvtd -q -o -f
#/home/hunner/local/bin/batt.sh &
xset -b
xset r rate 250 50
xset m 8/0.5 1.5
mpdscribble &
exec ssh-agent xmonad
