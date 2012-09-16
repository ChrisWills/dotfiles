#!/bin/bash

cat /dev/stdin | dzen2 -x $((`xdpyinfo | grep dim | awk '{ print $2 }' | awk -Fx '{ print $1 }'` - 2560)) -y '0' -h '16' -w $((1280 - 326)) -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'

#STATUSBAR_PID="$!"

#echo -n $STATUSBAR_PID > ${RUN_DIR}/statusbar.pid
