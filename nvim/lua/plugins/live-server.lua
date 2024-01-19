return {
    {
        "barrett-ruth/live-server.nvim",
        build = "npm install -g live-server",
        event = { "BufReadPre", "BufNewFile" },
        config = true,
    },
}
