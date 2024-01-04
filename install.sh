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
syncFileHome ".gitconfig"

if command -v yay &> /dev/null
then
    echo "yay already installed"
else
    printf "Installing 'YAY'"
    sudo pacman -S --needed git base-devel
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
fi

if command -v fish &> /dev/null
then
    echo "fish already installed"
else
    printf "Installing 'fish'"
    yay fish
fi
  
is_font_installed(){
    fontname=$1
    fc-list | grep -i "$fontname" >/dev/null
}
install_font(){
    fontname=$1
if ! is_font_installed $1; then 
    printf "Installing $fontname"
    yay "$fontname"
else
    printf "$fontname already installed\n"
fi
}

install_font "FiraCode Nerd Font"
install_font "D2Coding"

printf "${GREEN}Done${ENDCOLOR}\n"
