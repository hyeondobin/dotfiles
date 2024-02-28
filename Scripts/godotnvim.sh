#!/usr/bin/env bash
[ -n "$1" ] && file=$1
[ -n "$2" ] && line=$2
[ -n "$3" ] && col=$3
open_file_in_server() {
	"nvim" --server /tmp/nvim.pipe --remote-send "<ESC>:n $file<CR>:call cursor('$line', '$col')<CR>"
}
open_file_in_server "$1" "$2" "$3"
