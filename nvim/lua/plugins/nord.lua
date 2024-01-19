-- https://github.com/shaunsingh/nord.nvim
function ColorMyPencils(color)
    color = color or "nord"
    vim.cmd.colorscheme(color)

    vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    vim.api.nvim_set_hl(0, "NotifyBackground", { bg = "#434c5e" })
end

return {
    {
        "shaunsingh/nord.nvim",
        lazy = false,
        priority = 1000,
        config = function()
            -- require("nord").setup({})
            ColorMyPencils()
            vim.g.nord_disable_background = true
            vim.g.nord_contrast = true
            vim.g.cursorline_transparent = true
        end,
    },
}
