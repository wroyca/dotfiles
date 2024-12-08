---@module "fundo"

---@type LazyPluginSpec
local Spec = {
	"kevinhwang91/nvim-fundo", dependencies = "kevinhwang91/promise-async", event = "VeryLazy",

	init = function ()
		vim.o.undofile = true
	end,

	---@type FundoConfig
	opts = {
		limit_archives_size = 9999,
	},
}

return Spec
