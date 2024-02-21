#!/usr/bin/env bash
set -x

term_exec="bash"

nvim_exec="neovide"

server_path="/tmp/nvimsocket"

start_server() {
	"$term_exec" -c "$nvim_exec" "$1" -- --listen "$server_path"
}

open_file_in_server() {
	"nvim" --server "$server_path" --remote-send "<ESC>:n $1<CR>:call cursor($2, $3)<CR>"
}

if ! [ -e "$server_path" ]; then
	start_server "$1"
else
	open_file_in_server "$1" "$2" "$3"
fi
