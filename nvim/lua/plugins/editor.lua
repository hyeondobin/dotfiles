return {
    {
        "nvim-neo-tree/neo-tree.nvim",
        enabled = true,
        opts = {
            filesystem = {
                filtered_items = {
                    visible = true,
                },
                follow_current_file = {
                    enabled = true,
                    leave_dirs_open = false,
                },
                find_by_full_path_words = true,
            },
            event_handlers = {
                {
                    event = "file_opened",
                    handler = function(file_path)
                        require("neo-tree.command").execute({ action = "close" })
                    end,
                },
                {
                    event = "vim_buffer_enter",
                    handler = function()
                        if vim.bo.filetype == "neo-tree" then
                            vim.cmd("setlocal rnu")
                        end
                    end,
                },
            },
            window = {
                mappings = {},
            },
        },
    },
    {
        "folke/flash.nvim",
        enabled = false,
    },
}
