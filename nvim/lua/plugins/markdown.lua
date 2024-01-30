return {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = "cd app && yarn install",
    init = function()
        vim.g.mkdp_filetypes = { "markdown" }
    end,
    ft = { "markdown" },
    opts = {
        mkdp_auto_start = 1,
        mkdp_browser = "F:/Users/Dobin/AppData/Local/Vivaldi/Application/vivaldi.exe",
    },
    if vim.loop.os_uname().sysname == "Windows_NT" then
    {
        "epwalsh/obsidian.nvim",
        version = "*",
        lazy = true,
        event = {
                "BufReadPre /mnt/f/Users/Dobin/Obsidian/HyeonDobin/**.md",
                "BufNewFile /mnt/f/Users/Dobin/Obsidian/HyeonDobin/**.md",
        },
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
        opts = {
            workspaces = {
                {
                    name = "HyeonDobin",
                    path = "/mnt/f/Users/Dobin/Obsidian/HyeonDobin",
                },
            }
        }
    },
end
}
