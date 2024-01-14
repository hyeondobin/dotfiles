#!/usr/bin/env bash
if ! command -v inotifywait &>/dev/null; then
	printf "Installing inotify-tools\n"
	yay inotify-tools
fi

CONFIG_FILES="$HOME/.config/dunst/dunstrc"

trap "killall dunst" EXIT

while true; do
	dunst &
	notify-send "Reload dunstrc" "Reload dunst configuration"
	inotifywait -e create,modify $CONFIG_FILES
	killall dunst
done
