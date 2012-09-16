#!/bin/bash

/home/cwills/.xmonad/clock.sh | dzen2 -x $((`xdpyinfo | grep dim | awk '{ print $2 }' | awk -Fx '{ print $1 }'` - 2560 + 1280 - 136)) -y '0' -h '16' -w '136' -ta 'c' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'
