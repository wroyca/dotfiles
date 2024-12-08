---@module "mini.basics"

---@type LazyPluginSpec
local Spec = {
	"mini.basics", virtual = true, event = "VeryLazy",

	opts = {
		options = {
			basic = false,
			extra_ui = false,
			win_borders = "default",
		},

		mappings = {
			basic = true,
			option_toggle_prefix = [[\]],
			windows = true,
			move_with_alt = true,
		},

		autocommands = {
			basic = true,
			relnum_in_visual_mode = false,
		},

		silent = true,
	},
}

return Spec
