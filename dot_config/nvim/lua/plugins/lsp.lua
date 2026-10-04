return {
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "williamboman/mason.nvim", opts = {} },
			{
				"williamboman/mason-lspconfig.nvim",
				opts = {
					ensure_installed = { "lua_ls" },
				},
			},
		},
	},
	{
		"j-hui/fidget.nvim",
		opts = {},
	},
}
