---@type LazyPluginSpec
local Spec = {
  "folke/which-key.nvim", lazy = false, config = true, enabled = false,

  opts = {
    preset = "helix",
    filter = function(mapping)
      return mapping.desc and mapping.desc ~= ""
    end,
    icons = { mappings = false, separator = "│", },
    plugins = { mark = false, registers = false, spelling = { suggestions = 8, }, },
    win = { border = "single" },
    show_help = false,
    delay = function(ctx)
      return ctx.plugin and 0 or 500
    end,
    spec = {
      { "<leader>b", group = "Buffers" },
      { "<leader>l", group = "Language Server" },
    },
  },
}

return Spec
