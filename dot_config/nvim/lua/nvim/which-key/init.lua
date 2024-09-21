---@module 'which-key'
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "folke/which-key.nvim", event = "VeryLazy",

  ---@type wk.Opts
  opts = {
    preset = "helix",

    delay = function(ctx)
      return ctx.plugin and 0 or 500
    end,

    filter = function(mapping)
      return mapping.desc and mapping.desc ~= "" -- exclude mappings without a description
    end,

    plugins = {
      marks = false,
      registers = false,
      spelling = {
        enabled = false,
      },
      presets = {
        g = false,
        motions = true,
        nav = true,
        operators = true,
        text_objects = true,
        windows = true,
        z = false,
      },
    },

    win = {
      border = "single"
    },

    icons = {
      mappings = false,
      separator = "│",
    },

    show_help = false,
    show_keys = false,
  },
}

return Spec
