#!/usr/bin/env bash

set -e # stop script when error occurs

CWD=$(pwd)
BLUE="\e[34m"
YELLOW="\e[33m"
GREEN="\e[32m"
ENDCOLOR="\e[0m"

printf "# Syncing to home folder ... \n"

function syncFile() {
	local sourceFile="$1"
	if [ ! -d "$HOME/.config/${sourceFile}" ]; then
		printf "${BLUE}Symlink:${ENDCOLOR} ${sourceFile}\n"
		ln -sf "$CWD/${sourceFile}" "$HOME/.config/${sourceFile}"
		printf "Done\n"
	else
		printf "${YELLOW}Skipping${ENDCOLOR} ${sourceFile}\n"
	fi
}
function syncFileHome() {
	local sourceFile="$1"
	if [ ! -d "$HOME/${sourceFile}" ]; then
		printf "${BLUE}Symlink:${ENDCOLOR} ${sourceFile}\n"
		ln -sf "$CWD/${sourceFile}" "$HOME/${sourceFile}"
		printf "Done\n"
	else
		printf "${YELLOW}Skipping${ENDCOLOR} ${sourceFile}\n"
	fi
}

syncFile "fish"
syncFile "waybar"
syncFile "lazygit"
syncFile "kitty"
syncFile "hypr"
syncFile "fcitx"
syncFile "fcitx5"
syncFile "nvim"

syncFileHome "Scripts"

printf "${GREEN}Done${ENDCOLOR}\n"
