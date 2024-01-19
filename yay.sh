#!/usr/bin/env sh
# https://github.com/Jguer/yay

sudo pacman -S --needed git base-devel

if [[ ! -e /home/hyeondobin/Clones ]]; then
	mkdir /home/hyeondobin/Clones
fi

git clone https://aur.archlinux.org/yay.git /home/hyeondobin/Clones/yay
cd ~/Clones/yay
makepkg -si
