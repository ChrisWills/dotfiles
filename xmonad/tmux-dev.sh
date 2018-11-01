#!/bin/bash

session="dev"

if tmux list-sessions | grep -q $session ; then
    tmux -2 attach-session -t $session
else
    tmux new-session -d -s $session

    tmux selectp -t 1
    tmux send-keys "emacs -nw" Enter

    tmux new-window -t $session:1
    tmux splitw -v

    tmux select-window -t $session:0

    tmux -2 attach-session -t $session
fi
