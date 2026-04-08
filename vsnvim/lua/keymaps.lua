vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
vim.keymap.set("n", "<leader>e", function()
    require("vscode").action('workbench.view.explorer')
end)
