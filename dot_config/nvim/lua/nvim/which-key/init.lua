---@module 'which-key'
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "folke/which-key.nvim", event = "VeryLazy", opts_extend = { "spec" },

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
    spec = {
      { "<leader>l",  group = "lsp" },
      { "<Leader>lf", "<cmd>lua vim.lsp.buf.format({async = true})<CR>", desc = "Format buffer" },
    },
  },
}

return Spec