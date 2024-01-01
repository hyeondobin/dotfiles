#!/usr/bin/env bash

set -e # stop script when error occurs

CWD=$(pwd)

printf "# Syncing to home folder ... \n"

function syncFile() {
	local sourceFile="$1"
	if [ ! -d "$HOME/.config/${sourceFile}" ]; then
		printf "Symlink: ${sourceFile}\n"
		ln -sf "$CWD/${sourceFile}" "$HOME/.config/${sourceFile}"
		printf "Done\n"
	else
		printf "Skipping ${sourceFile}\n"
	fi
}
function syncFileHome() {
	local sourceFile="$1"
	if [ ! -d "$HOME/${sourceFile}" ]; then
		printf "Symlink: ${sourceFile}\n"
		ln -sf "$CWD/${sourceFile}" "$HOME/${sourceFile}"
		printf "Done\n"
	else
		printf "Skipping ${sourceFile}\n"
	fi
}

syncFile "fish"
syncFile "waybar"
syncFile "lazygit"
syncFile "kitty"
syncFile "hypr"
syncFile "fcitx"
syncFile "fcitx5"

syncFileHome "Scripts"

printf "Done\n"
