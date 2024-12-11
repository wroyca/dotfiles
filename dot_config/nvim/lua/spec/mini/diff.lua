---@module "mini.diff"

---@type LazyPluginSpec
local Spec = {
  "mini.diff", virtual = true, event = "VeryLazy",

  init = function ()
    vim.o.signcolumn = "yes:1"
  end,

  keys = {
    { "go", function () MiniDiff.toggle_overlay (0) end, mode = "n", desc = "Toggle hunks overlay" },
  },

  opts = {
    delay = {
      text_change = 0,
    },
    view = {
      style = "sign",
      signs = {
        add = "┃",
        change = "┃",
        delete = "┃",
      },
    },
  },

  -- opts shouldn't call setup, as mini modules self-export through _G.
  config = function (_, opts)
    require ("mini.diff").setup (opts)
  end,
}

return Spec
