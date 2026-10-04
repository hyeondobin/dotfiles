vim.api.nvim_create_autocmd("TextYankPost", {
	desc = "Highlight when yanking text",
	group = vim.api.nvim_create_augroup("config-highlight-yank", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

local fold_group = vim.api.nvim_create_augroup("AutoSaveFolds", { clear = true })

vim.api.nvim_create_autocmd({ "BufWinLeave", "BufWritePost" }, {
	group = fold_group,
	callback = function(args)
		if vim.bo[args.buf].buftype == "" and vim.api.nvim_buf_get_name(args.buf) ~= "" then
			vim.cmd.mkview({ mods = { emsg_silent = true } })
		end
	end,
})

vim.api.nvim_create_autocmd("BufWinEnter", {
	group = fold_group,
	callback = function(args)
		if vim.bo[args.buf].buftype == "" and vim.api.nvim_buf_get_name(args.buf) ~= "" then
			vim.cmd.loadview({ mods = { emsg_silent = true } })
		end
	end,
})
