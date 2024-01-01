-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local opts = { silent = true, remap = false }

-- add ; to end of line
vim.keymap.set("i", ";<Space>", "; ", opts)
vim.keymap.set("i", ";", "<C-c>A;", opts)

-- move line up/down with Alt
vim.keymap.set("n", "<A-j>", "ddp", opts)
vim.keymap.set("n", "<A-k>", "ddP", opts)
