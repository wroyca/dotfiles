---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, event = "VeryLazy",

  config = function(_, opts)
    local misc = require "mini.misc"
    misc.setup(opts)
    misc.setup_auto_root()
    misc.setup_restore_cursor()
  end
}

return Spec
