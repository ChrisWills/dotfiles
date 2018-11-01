#!/bin/bash

xrdb -merge /home/cwills/.Xdefaults
xterm -e '/home/cwills/.xmonad/tmux-dev.sh'
