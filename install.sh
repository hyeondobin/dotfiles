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
	if [ ! -e "$HOME/${sourceFile}" ]; then
		printf "${BLUE}Symlink:${ENDCOLOR} ${sourceFile}\n"
		ln -sf "$CWD/${sourceFile}" "$HOME/${sourceFile}"
		printf "Done\n"
	else
		printf "${YELLOW}Skipping${ENDCOLOR} ${sourceFile}\n"
	fi
}

function install() {
	local program=$1
	if command -v "${program}" &>/dev/null; then
		echo "${program} already installed"
	else
		printf "Installing '%s'", "$program"
		yay "${program}"
	fi
}

installDifName() {
	local program=$1
	local command=$2
	if command -v "${command}" &>/dev/null; then
		echo "${program} already installed"
	else
		printf "Installing '${program}'\n"
		yay "${program}"
	fi
}

is_font_installed() {
	fontname=$1
	fc-list | grep -i "$fontname" >/dev/null
}

install_font() {
	fontname=$1
	if [ -e "$2" ]; then
		fontcommand=$2
		if ! is_font_installed "$1"; then
			printf "Installing $fontname\n"
			yay "$fontcommand"
		fi
	fi
	if ! is_font_installed "$1"; then
		printf "Installing $fontname\n"
		yay "$fontname"
	else
		printf "$fontname already installed\n"
	fi
}

syncFile "fish"
syncFile "waybar"
syncFile "lazygit"
syncFile "kitty"
syncFile "hypr"
syncFile "fcitx"
syncFile "fcitx5"
syncFile "wallpapers"
syncFile "dunst"
syncFile "rofi"
syncFile "tmux"
syncFile "pipewire"
syncFile "blender"
syncFile "starship.toml"

syncFileHome "Scripts"
syncFileHome ".gitconfig"

# Update pacman DB
read -r -p "Update package DB? (y/n) " answer
answer=${answer,,}
if [[ "$answer" =~ ^(y| )$ ]]; then
	sudo pacman -Syu
else
	printf "Skipping package DB update\n"
fi

if command -v yay &>/dev/null; then
	echo "yay already installed"
else
	printf "Installing 'YAY'"
	sudo pacman -S --needed git base-devel
	git clone https://aur.archlinux.org/yay.git
	cd yay
	makepkg -si
fi

if command -v caps2esc &>/dev/null; then
	echo "caps2esc already installed"
else
	printf "Installing 'caps2esc'."
	yay caps2esc
	systemctl enable udevmon.service
fi

if [ ! -e "/etc/interception/udevmon.d/caps2esc.yaml" ]; then
	printf "${BLUE}Symlink:${ENDCOLOR} caps2esc job config.\n"
	sudo ln -sf "$CWD/caps2esc.yaml" "/etc/interception/udevmon.d/caps2esc.yaml"
	printf "Done\n"
else
	printf "${YELLOW}Skipping${ENDCOLOR} caps2esc job config.\n"
fi

install "fish"
install "wlogout"
install "hyprpaper"
install "fzf"
install "fd"
install "bat"
install "eza"
install "tmux"
install "man"
install "rofi"
install "lazygit"
install "node"
install "rustup"
install "cmake"
install "zoxide"
install "pulsemixer"
install "starship"

installDifName "github-cli" "gh"
installDifName "ripgrep" "rg"

install_font "FiraCode Nerd Font"
install_font "D2Coding"
install_font "font awesome" "ttf-font-awesome"

if command -v live-server &>/dev/null; then
	printf "live-server already installed\n"
else
	printf "Installing live-server"
	sudo npm install -g live-server
fi

printf "${GREEN}Done${ENDCOLOR}\n"
