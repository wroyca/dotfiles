---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, event = "VeryLazy",

  config = function(_, opts)
    require("mini.misc").setup(opts)
    MiniMisc.setup_auto_root()
    MiniMisc.setup_restore_cursor()
  end,
}

return Spec
