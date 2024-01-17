if status is-interactive
    and not set -q TMUX
    # Commands to run in interactive sessions can go here
    exec tmux new-session -As PanRuyal
end
