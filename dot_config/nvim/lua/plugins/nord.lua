vim.g.nord_contrast = true
vim.g.cursorline_transparent = true

return {
  {
    "shaunsingh/nord.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      -- require("nord").setup({})
      vim.cmd.colorscheme("nord")
    end,
  },
}
