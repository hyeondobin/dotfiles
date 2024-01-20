#!/usr/bin/env bash

clone() {
	git clone https://github.com/$1.git ~/Clones/$2
}
jobsLeft=true
while [[ $jobsLeft = true ]]; do

	printf "Choose which to clone\n"
	printf "\n"
	printf "1. z.lua\n"
	printf "2. rofi theme collection\n"
	printf "3. tmux plugin manager(tpm)\n"
	printf "4. hyeondobin obsidian\n"
	printf "5. catppuccin\n"

	read -p "Choose (eg: 1 or [a]ll) >> " answer
	if command -v git &>/dev/null; then
		case $answer in
		1)
			clone skywind3000/z.lua z.lua
			jobsLeft=false
			;;
		2)
			if [[ ! -e ~/Clones/rofi-themes-collection ]]; then
				clone newmanls/rofi-themes-collection rofi-themes-collection
			fi
			mkdir -p ~/.local/share/rofi/themes
			cp ~/Clones/rofi-themes-collection/themes/rounded-common.rasi ~/.local/share/rofi/themes/
			cp ~/Clones/rofi-themes-collection/themes/rounded-nord-dark.rasi ~/.local/share/rofi/themes/
			jobsLeft=false
			;;
		3)
			clone tmux-plugins/tpm tpm
			jobsLeft=false
			;;
		4)
			clone hyeondobin/obsidianRepo obsidianRepo
			jobsLeft=false
			;;
		5)
			clone catppuccin/hyprland hyprland
			jobsLeft=false
			;;
		a)
			clone skywind3000/z.lua z.lua
			clone newmanls/rofi-themes-collection rofi-themes-collection
			jobsLeft=false
			;;
		*)
			printf "Choose right answer\n"
			printf "\n"
			;;
		esac
	fi

done
