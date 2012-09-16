#!/bin/bash

PIDS=""
RUN_DIR="$HOME/.xmonad/run"

for file in ${RUN_DIR}/*; do 
  kill -9 `cat $file`
done

