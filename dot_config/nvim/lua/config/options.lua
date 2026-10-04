---@diagnostic disable: assign-type-mismatch
local op = vim.opt

-- tab / indent
op.tabstop = 4
op.shiftwidth = 4
op.softtabstop = 4
op.expandtab = true
op.autoindent = true
op.smartindent = true

-- search options
op.ignorecase = true
op.smartcase = true
op.hlsearch = true
op.incsearch = true
op.wrapscan = true
op.inccommand = "split"

-- appearance
op.number = true
op.relativenumber = true
op.termguicolors = true
op.signcolumn = "yes"
op.colorcolumn = "100"
op.cmdheight = 1
op.scrolloff = 15
op.completeopt = "menu,menuone,popup,noselect,fuzzy" -- blink.cmp는 사용하지 않지만 일단 추가.
op.cursorline = true
op.cursorcolumn = false
op.winborder = vim.g.borderStyle
op.list = true
op.viewoptions = { "cursor", "folds" }

-- behavior
op.hidden = true
op.errorbells = false
op.swapfile = false
op.backup = false
op.undodir = vim.fn.expand("~/.vim/undodir")
op.undofile = true
op.backspace = "indent,eol,start"
op.splitright = true
op.splitbelow = true
op.autochdir = false
op.iskeyword:append("-")
op.isfname:append("@-@")
op.mouse:append("a")
op.modifiable = true
op.encoding = "UTF-8"
op.updatetime = 200
op.conceallevel = 0
op.timeoutlen = 400

-- set options for neovide
if vim.g.neovide then
	op.guifont = "D2KodingLigature Nerd Font:h16"
	op.linespace = 0
	vim.g.neovide_scale_factor = 1.0
end
