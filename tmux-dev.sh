#!/bin/bash

session="dev"

tmux new-session -d -s $session

tmux selectp -t 1
tmux send-keys "emacs -nw" Enter

tmux new-window -t $session:1
tmux splitw -v

tmux select-window -t $session:0

tmux attach-session -t $session
