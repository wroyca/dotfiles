---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", dev = true, event = "VimEnter",

  config = function()
    local misc = require [[mini.misc]]
    misc.setup()
    misc.setup_auto_root()
    misc.setup_restore_cursor()
    misc.setup_termbg_sync()
  end
}

return Spec
