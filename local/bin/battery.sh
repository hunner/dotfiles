#!/bin/sh
#
# by lyon8 (lyon8@gmx.net)
# show your laptop battery state in dzen

BG='#000'  # dzen backgrounad
FG='#ddd'  # dzen foreground
W=157      # width of the dzen bar
GW=100     # width of the gauge
GFG='#057' # color of the gauge
GH=12      # height of the gauge
GBG='#333' # color of gauge background
X=2722     # x position
Y=1        # y position
FN='"xft:Liberation Mono:size=7"' # font

STATEFILE='/sys/class/power_supply/BAT0/status' # battery's state file
CAPFILE='/sys/class/power_supply/BAT0/capacity'   # battery's capacity file

MIDBAT=65        # percentage of battery life marked as low
MIDCOL='#dddd47' # color when battery is low
LOWBAT=30        # percentage of battery life marked as low
LOWCOL='#dd4747' # color when battery is low
TIME_INT=15      # time interval in seconds

gdbar="$([ -x /opt/dzen/bin/gdbar ] && echo '/opt/dzen/bin/gdbar' || echo 'gdbar')"

# Today's date & current time
#DATE_FORMAT="%d %b %Y %H:%M"
DATE_FORMAT="%H:%M"

while true; do
    # find remaining power
    RPERC=`cat $CAPFILE`;

    # draw the bar and pipe everything into dzen
    if [ $RPERC -gt $MIDBAT ]; then GFGC=$GFG; fi
    if [ $RPERC -le $MIDBAT ]; then GFGC=$MIDCOL; fi
    if [ $RPERC -le $LOWBAT ]; then GFGC=$LOWCOL; fi

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
done | dzen2 -ta c -tw $W -y $Y -x $X -fg "$FG" -bg "$BG" -fn "$FN" -h 18 -e "sigusr1=raise"
