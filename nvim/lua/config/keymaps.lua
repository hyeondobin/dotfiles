-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
local opts = { silent = true, remap = false }
local wk = require("which-key")

-- add ; to end of line
vim.keymap.set("i", "<C-S>;", "<C-o>A;", opts)
vim.keymap.set("i", "<C-S>,", "<C-o>A,", opts)

-- move line up/down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- https://github.com/folke/which-key.nvim?tab=readme-ov-file#%EF%B8%8F-mappings
if vim.loop.os_uname().sysname == "Linux" then
    wk.register({
        p = {
            name = "preview",
            s = { "<cmd>LiveServerStart<CR>", "Start LiveServer" },
            c = { "<cmd>LiveServerStop<CR>", "Stop LiveServer" },
            m = {
                name = "markdown",
                s = { "<cmd>MarkdownPreview<CR>", "Start MarkdownPreview" },
                c = { "<cmd>MarkdownPreviewStop<CR>", "Stop MarkdownPreview" },
            },
        },
    }, { prefix = "<leader>" })
end

if vim.loop.os_uname().sysname == "Windows_NT" then
    wk.register({
        p = {
            name = "preview",
            m = {
                name = "markdown",
                s = { "<cmd>MarkdownPreview<CR>", "Start MarkdownPreview" },
                c = { "<cmd>MarkdownPreviewStop<CR>", "Stop MarkdownPreview" },
            },
        },
    }, { prefix = "<leader>" })
end

vim.keymap.set("n", "<C-h>", "<cmd>TmuxNavigateLeft<CR>", { desc = "window left" })
vim.keymap.set("n", "<C-j>", "<cmd>TmuxNavigateDown<CR>", { desc = "window down" })
vim.keymap.set("n", "<C-k>", "<cmd>TmuxNavigateUp<CR>", { desc = "window up" })
vim.keymap.set("n", "<C-l>", "<cmd>TmuxNavigateRight<CR>", { desc = "window right" })
