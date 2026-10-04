return {
    "stevearc/oil.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    lazy = false,
    opts = {
        columns = {
            "icon",
            "mtime",
        },
        skip_confirm_for_simple_edits = true,
        watch_for_changes = true,
        view_options = {
            show_hidden = true,
            is_always_hidden = function(name, _)
                if name == ".." then
                    return true
                else
                    return false
                end
            end,
        },
        win_options = {
            signcolumn = "yes:2",
        },
    },
    keys = {
        { "-", "<cmd>Oil --float<CR>", desc = "Open Oil" },
    },
}
