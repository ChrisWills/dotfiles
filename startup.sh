#!/bin/bash

sw=$1
rundir="$HOME/.xmonad/run/"

# clockbar
clockbarstart=`echo "$sw - 136" | bc`

while true; do date +'%a %b %d %l:%M%p'; sleep 30; done | dzen2 -x "$clockbarstart" -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*' &

clockpid=$!

echo "$clockpid" > "$rundir/clock.pid"


# tray
traystart=`echo "$sw - 326" | bc`

/usr/bin/stalonetray --geometry 12x1+${traystart}+0 --max-geometry 12x1+" ++ (show traystart) ++ "+0 --background '#161616' --icon-size 16 --icon-gravity NE --kludges=force_icons_size &

traypid=$!

echo "$traypid" > "$rundir/tray.pid"


# battery app
$HOME/bin/batt_stat.rb &

echo "$!" > "$rundir/batt.pid"

# nm app
nm-applet &

echo "$!" > "$rundir/nm-applet.pid"

# xscreensaver
xscreensaver &

echo "$!" > "$rundir/xscreensaver.pid"

xset r rate 200 60
xmodmap $HOME/.Xmodmap
feh --bg-fill $HOME/.wallpaper/current
xbacklight -set 70
