#!/bin/bash

rundir="$HOME/.xmonad/run/"

for a in clock.pid tray.pid batt.pid nm-applet.pid xscreensaver.pid; do
  kill $(cat "$rundir/$a")
done
