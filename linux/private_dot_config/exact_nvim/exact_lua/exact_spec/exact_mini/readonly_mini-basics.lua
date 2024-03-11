---@type LazyPluginSpec
return {
  [[mini.basics]],
  event = [[VeryLazy]],
  opts = {
    options = {
      basic = false,
      extra_ui = false,
      win_borders = [[single]]
    },
    mappings = {
      basic = false,
      windows = false,
      option_toggle_prefix = [[]],
      move_with_alt = false
    },
    autocommands = {
      basic = true,
      relnum_in_visual_mode = true
    }
  }
}
