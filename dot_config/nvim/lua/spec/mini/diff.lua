---@type LazyPluginSpec
return {
  [[mini.diff]], event = [[VeryLazy]],

  keys = function()
    local diff = require [[mini.diff]]
    return {
      { [[go]], diff.toggle_overlay, mode = [[n]], desc = [[Toggle hunks overlay]] },
    }
  end,

  init = function()
    vim.o.signcolumn = [[yes:1]]
  end,

  opts = {
    delay = {
      text_change = 0
    },
    view = {
      style = [[sign]],
      signs = {
        add = [[┃]],
        change = [[┃]],
        delete = [[┃]]
      }
    }
  }
}
