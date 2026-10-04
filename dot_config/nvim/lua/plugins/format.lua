return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	opts = {
		formatters_by_ft = {
			lua = { "stylua" },
			json = { "prettierd", "prettier", stop_after_first = true },
			jsonc = { "prettierd", "prettier", stop_after_first = true },
			css = { "prettierd", "prettier", stop_after_first = true },
		},
		format_on_save = {
			timeout_ms = 500,
			async = false,
			quiet = false,
			lsp_format = "fallback",
		},
	},
}
