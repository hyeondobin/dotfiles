#!/usr/bin/env bash

set -e # stop script when error occurs

CWD=$(pwd)

printf "# Syncing to home folder ... \n"

function syncFile() {
	local sourceFile="$1"
	printf "Symlink: ${sourceFile}\n"
	ln -sf "$CWD/${sourceFile}" "$HOME/.config/${sourceFile}"
	printf "Done\n"
}
function syncFileHome() {
	local sourceFile="$1"
	ln -sf "$CWD/${sourceFile}" "$HOME/${sourceFile}"
}

syncFile "fish"
syncFile "waybar"
syncFile "lazygit"
syncFile "kitty"

syncFileHome "Scripts"

printf "Done\n"
