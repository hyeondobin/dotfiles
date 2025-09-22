#!/usr/bin/env bash

nvimTitle = "^(nvim).*"

function handle() {
    case $1 in
        windowtitlev2*) 
    esac
}

socat - "UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock" | while read -r line; do handle "$line"; done
