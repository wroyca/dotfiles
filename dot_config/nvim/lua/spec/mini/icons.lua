---@module "mini.icons"

---@type LazyPluginSpec
local Spec = {
	"mini.icons", virtual = true,

	-- opts shouldn't call setup, as mini modules self-export through _G.
	config = function (_, opts)
		require ("mini.icons").setup (opts)

		-- Mock exported functions of 'nvim-tree/nvim-web-devicons' plugin. It will
		-- mock all its functions which return icon data by using |MiniIcons.get()|
		-- equivalent.
		MiniIcons.mock_nvim_web_devicons ()
	end,
}

return Spec