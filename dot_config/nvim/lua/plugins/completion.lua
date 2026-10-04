return {
	"saghen/blink.cmp",
	dependencies = {
		"saghen/blink.lib",
		"rafamadriz/friendly-snippets",
	},
	version = "1.*",
	-- build = function()
	-- 	require("blink.cmp").build():pwait()
	-- end,
	opts = {
		keymap = { preset = "default" },
		completion = {
			accept = {
				auto_brackets = { enabled = false },
			},
			documentation = { auto_show = false },
		},
		sources = { default = { "lsp", "path", "snippets", "buffer" } },
	},
}
