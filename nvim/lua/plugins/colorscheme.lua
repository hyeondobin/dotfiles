-- https://github.com/catppuccin/nvim
function ColorMyPencils(color)
    color = color or "catppuccin"
    vim.cmd.colorscheme(color)

    -- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
    -- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
    -- vim.api.nvim_set_hl(0, "NotifyBackground", { bg = "#434c5e" })
end

return {
    {
        "catppuccin/nvim",
        name = "catppuccin-mocha",
        priority = 1000,
        enabled = true,
        lazy = false,
        config = function()
            -- require("nord").setup({})
            transparent_background = true
            ColorMyPencils()
        end,
    },
}
