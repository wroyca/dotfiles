---@type LazyPluginSpec
return {
  [[mini.misc]], event = [[VeryLazy]],

  config = function()
    local misc = require [[mini.misc]]
    misc.setup()
    misc.setup_auto_root()
    misc.setup_restore_cursor()
  end
}
