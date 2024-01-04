#!/bin/bash
if ! command -v inotifiwait &> /dev/null
then
    printf "Installing inotify-tools"
    yay inotify-tools
fi

CONFIG_FILES="$HOME/.config/waybar/config $HOME/.config/waybar/style.css"

trap "killall waybar" EXIT

while true; do
	waybar &
	inotifywait -e create,modify $CONFIG_FILES
	killall waybar
done
