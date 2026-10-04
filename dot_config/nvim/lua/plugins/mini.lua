return {
	{
		"nvim-mini/mini.ai",
		version = false,
		opts = {},
	},
	{
		"nvim-mini/mini.files",
		version = false,
		opts = {
			preview = true,
		},
		keys = {
			{
				"<leader>e",
				function()
					require("mini.files").open()
				end,
				desc = "Mini Files",
			},
		},
	},
	{
		"nvim-mini/mini.indentscope",
		version = false,
		opts = {},
	},
	{
		"nvim-mini/mini.pairs",
		version = false,
		event = { "BufReadPost" },
		opts = {},
	},
	{
		"nvim-mini/mini.surround",
		version = false,
		opts = {
			mappings = {
				add = "<leader>sa",
				delete = "<leader>sd",
				find = "<leader>sf",
				find_left = "<leader>sF",
				highlight = "<leader>sh",
				replace = "<leader>sr",
				update_n_lines = "<leader>sn",

				suffix_last = "l",
				suffix_next = "n",
			},
		},
		config = true,
		event = "BufReadPost",
	},
	{
		"nvim-mini/mini.tabline",
		version = false,
		opts = {},
	},
}
