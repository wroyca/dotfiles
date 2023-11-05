---@type LazyPluginSpec
return {
  [[echasnovski/mini.basics]],
  event = [[VeryLazy]],

  opts = {
    options = {
      basic = false,
      extra_ui = false,
      win_borders = [[single]]
    },

    mappings = {
      basic = false,
      option_toggle_prefix = [[\]],
      windows = true
    },

    autocommands = {
      basic = true,
      relnum_in_visual_mode = false
    }
  }
}
