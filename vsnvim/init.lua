-- 현재 파일의 위치를 런타임 패스에 추가.
local cur_file = debug.getinfo(1, "S").source:sub(2)
local vsnvim_dir = vim.fn.fnamemodify(cur_file, ":h")
vim.opt.runtimepath:prepend(vsnvim_dir)

require("options")
require("keymaps")
