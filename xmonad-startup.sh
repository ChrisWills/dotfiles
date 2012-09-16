#!/bin/bash
# commands
RUN_DIR="$HOME/.xmonad/run"

screen_w=`xdpyinfo | grep dim | awk '{ print $2 }' | awk -Fx '{ print $1 }'`
screen_h=`xdpyinfo | grep dim | awk '{ print $2 }' | awk -Fx '{ print $2 }'`
tray_start=$(($screen_w - 2560 + 1280 - 326))
#MAIN_BAR="dzen2 -x '0' -y '0' -h '16' -w '1040' -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
#CLOCK="bash /home/cwills/.xmonad/clock.sh | dzen2 -x '1230' -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
TRAY="/usr/bin/stalonetray --geometry 12x1+${tray_start}+0 --max-geometry 12x1+${tray_start}+0 --background #161616 --icon-size 16 --icon-gravity NE --kludges=force_icons_size"
NET="/usr/bin/nm-applet"
POWER="/usr/bin/gnome-power-manager"
SOUND="/usr/bin/gnome-volume-control-applet"

xmodmap ~/.Xmodmap 

xrdb -merge ~/.Xdefaults

xset r rate 200 60

#xcompmgr &
#COMPIZ_PID="$!"

#$MAIN_BAR &
#MAIN_BAR_PID="$!"

#$CLOCK &
#CLOCK_PID

$TRAY &
TRAY_PID="$!"

$CLOCKBAR &
CLOCKBAR_PID="$!"

#(sleep 3 && $POWER )&
#POWER_PID="$!"

#(sleep 3 && $NET )&
#NET_PID="$!"

#echo -n $MAIN_BAR_PID > ${RUN_DIR}/mbar.pid
#echo -n $CLOCK_PID > ${RUN_DIR}/clock.pid
echo -n $TRAY_PID > ${RUN_DIR}/tray.pid
echo -n $CLOCKBAR_PID > ${RUN_DIR}/clockbar.pid

#echo -n $POWER_PID > ${RUN_DIR}/power.pid
#echo -n $NET_PID > ${RUN_DIR}/net.pid
#echo -n $COMPIZ_PID > ${RUN_DIR}/compiz.pid
