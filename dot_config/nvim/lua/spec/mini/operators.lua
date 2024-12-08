---@module "mini.operators"

---@type LazyPluginSpec
local Spec = {
	"mini.operators", virtual = true, opts = {},

	keys = {
		{ "g=", mode = { "n", "x" } },
		{ "gx", mode = { "n", "x" } },
		{ "gm", mode = { "n", "x" } },
		{ "gr", mode = { "n", "x" } },
		{ "gs", mode = { "n", "x" } },
	},
}

return Spec
