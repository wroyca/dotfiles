---@module "bqf"
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
	"kevinhwang91/nvim-bqf", ft = "qf",

	---@type BqfConfig
	opts = {
		auto_resize_height = true,
		preview = {
			border = "single",
			show_title = false,
		},
	},
}

return Spec
