return {
	{
		"folke/snacks.nvim",
		enabled = true,
		priority = 1000,
		lazy = false,
		dependencies = { "nvim-tree/nvim-web-devicons" },
		keys = {
			{
				"<leader>fd",
				function()
					Snacks.picker.diagnostics()
				end,
				desc = "document diagnostics",
			},
			{
				"<leader>ffc",
				function()
					Snacks.picker.files({ cwd = vim.fn.stdpath("config") })
				end,
				desc = "find files; config",
			},
			{
				"<leader>ffg",
				function()
					require("nvim-rooter").rooter_default()
					Snacks.picker.grep()
				end,
				desc = "find file; grep",
			},
			{
				"<leader>ffr",
				function()
					Snacks.picker.recent()
				end,
				desc = "find file; recent",
			},
			{
				"<leader>fh",
				function()
					Snacks.picker.help()
				end,
				desc = "find help",
			},
			{
				"<leader>fk",
				function()
					Snacks.picker.keymaps()
				end,
				desc = "find keymaps",
			},
			{
				"<leader>fs",
				function()
					Snacks.picker.lsp_symbols()
				end,
				desc = "find lsp symbols",
			},
			{
				"<leader>f;",
				function()
					Snacks.picker.command_history()
				end,
				desc = "find command history",
			},
			{
				"<leader>f:",
				function()
					Snacks.picker.commands()
				end,
				desc = "find commands",
			},
			{
				"<leader>fo",
				function()
					Snacks.picker.current()
				end,
				desc = "find commands",
			},
			{
				"<leader>gb",
				function()
					Snacks.picker.git_diff()
				end,
				desc = "git blame line",
			},
			{
				"<leader>gl",
				function()
					require("nvim-rooter").rooter_default()
					Snacks.lazygit()
				end,
				desc = "Open lazygit",
			},
			{
				"<leader>gs",
				function()
					Snacks.picker.git_status()
				end,
				desc = "find git status",
			},
			{
				"<leader>nh",
				function()
					Snacks.notifier.show_history()
				end,
				desc = "Notification history",
			},
			{
				"<leader>nl",
				function()
					openNotify("last")
				end,
				mode = { "n", "v" },
				desc = "Notification last",
			},
			{
				"<C-p>",
				function()
					Snacks.picker.smart()
				end,
				desc = "find files",
			},
			{
				"<m-r>",
				function()
					Snacks.picker.resume()
				end,
				desc = "Resume",
			},
		},
		opts = {
			dashboard = { enabled = true },
		},
	},
}
