---@module "mini.diff"

---@type LazyPluginSpec
local Spec = {
  "mini.diff", dev = true, event = "VeryLazy", enabled = true,

  init = function()
    vim.o.signcolumn = "yes:1"
  end,

  keys = function()
    local diff = require "mini.diff"
    return {
      { "go", diff.toggle_overlay, mode = "n", desc = "Toggle hunks overlay" },
    }
  end,

  opts = {
    delay = {
      text_change = 0
    },
    view = {
      style = "sign",
      signs = {
        add = "┃", change = "┃", delete = "┃"
      }
    }
  }
}

return Spec
