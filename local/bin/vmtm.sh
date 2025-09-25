#!/usr/bin/env bash

SESSION="vm"

tmux new-session -d -s $SESSION -n root "sudo su -"
tmux new-window -t $SESSION:1 -n d-c -c ~/Documents/work/git/backend
tmux new-window -t $SESSION:2 -n backend -c ~/Documents/work/git/backend
tmux new-window -t $SESSION:3 -n frontend -c ~/Documents/work/git/frontend
tmux new-window -t $SESSION:4 -n k3d -c ~/Documents/work/git/installation/k3d
tmux new-window -t $SESSION:5 -n installation -c ~/Documents/work/git/installation

tmux select-window -t $SESSION:0
tmux attach-session -t $SESSION
