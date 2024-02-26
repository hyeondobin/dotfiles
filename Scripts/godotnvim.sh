#!/usr/bin/env bash
set -x

nvim_exec="neovide"

server_path="/tmp/nvimsocket"
if [ -z "$2" ]; then
	line=$2
else
	line=0
fi
if [ -z "$3" ]; then
	col=$3
else
	col=0
fi

start_server() {
	"$nvim_exec" "$1" -- "+call cursor($line, $col)" --listen "$server_path"
}

open_file_in_server() {
	"nvim" --server "$server_path" --remote-send "<ESC>:n $1<CR>:call cursor('$2', '$3')<CR>"
}

if ! [ -e "$server_path" ]; then
	start_server "$1"
else
	open_file_in_server "$1" "$2" "$3"
fi
