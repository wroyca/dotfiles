---@type LazyPluginSpec
return {
  [[mini.misc]],
  dev = true,
  event = [[VeryLazy]],

  config = function()
    local MiniMisc = require [[mini.misc]]
    MiniMisc.setup()
    MiniMisc.setup_auto_root()
    MiniMisc.setup_restore_cursor()
  end
}
