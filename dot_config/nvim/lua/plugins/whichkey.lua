return {
	"folke/which-key.nvim",
	event = "VimEnter",
	opts = {
		preset = "helix",
		plugins = { spelling = true },
		spec = {
			{
				mode = "n",
				{ "<leader>", group = "" },
				{ "<leader>b", group = "Buffer" },
				{ "<leader>f", group = "Find" },
				{ "<leader>ff", group = "Find files" },
				{ "<leader>g", group = "Git" },
				{ "<leader>n", group = "Notification" },
			},
		},
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
	},
}
