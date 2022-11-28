#!/bin/sh
#
# by lyon8 (lyon8@gmx.net)
# show your laptop battery state in dzen

BG='#000'  # dzen backgrounad
FG='#ddd'  # dzen foreground
W=112      # width of the dzen bar
H=12       # height of the dzen bar
GW=80     # width of the gauge
#GFG='#5e81ac' # color of the gauge
GFG='#379' # color of the gauge
GH=10      # height of the gauge
GBG='#333' # color of gauge background
#X=1807     # x position
X="$(expr "$(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+)x[0-9]+.*$/\1/')" - $W - 2)"     # x position
Y=2        # y position
FN='xft:Liberation Mono:size=8' # font

STATEFILE='/sys/class/power_supply/BAT0/status' # battery's state file
CAPFILE='/sys/class/power_supply/BAT0/capacity'   # battery's capacity file

CHRGCOL='#99ff00' # color when battery is charging
LOWBAT=15        # percentage of battery life marked as low
LOWCOL='#dd4747' # color when battery is low
TIME_INT=15      # time interval in seconds

gdbar="$([ -x /opt/dzen/bin/gdbar ] && echo '/opt/dzen/bin/gdbar' || echo 'gdbar')"

# Today's date & current time
#DATE_FORMAT="%d %b %Y %H:%M"
DATE_FORMAT="%H:%M"

while true; do
    # find remaining power
    RPERC=`cat $CAPFILE`;
    STATE=`cat $STATEFILE`;

    # draw the bar and pipe everything into dzen
    if [ $RPERC -gt $LOWBAT ]; then GFGC=$GFG; fi
    if [ $RPERC -le $LOWBAT ]; then GFGC=$LOWCOL; fi
    if [ $STATE = "Charging" ]; then GFGC=$CHRGCOL; fi

    case $RPERC in
        $LOWBAT)
            message="Battery getting low, better plug in..."
            level="normal"
            notify $level "$message"
        ;;
        15)
            message="Battery dangerously low! Plug in AC!"
            level="critical"
            notify $level "$message"
        ;;
    esac

    echo -n "$(date +"$DATE_FORMAT") "
    eval echo $RPERC | $gdbar -h $GH -w $GW -fg $GFGC -bg $GBG
    sleep $TIME_INT;
done | dzen2 -ta c -tw $W -y $Y -x $X -fg "$FG" -bg "$BG" -fn "$FN" -h $H -e "sigusr1=raise"
