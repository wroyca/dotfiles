---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[f-person/auto-dark-mode.nvim]],
    name = [[colorscheme]],
    lazy = false,
    dependencies = {
      {
        [[echasnovski/mini.colors]],
        config = true
      }
    },

    ---@diagnostic disable-next-line: assign-type-mismatch
    config = {
      set_dark_mode = function()
        vim.cmd([[Colorscheme dark]])
      end,
      set_light_mode = function()
        vim.cmd([[Colorscheme light]])
      end
    }
  },

  -- Every now and then, I might desire a change of pace.
  {
    [[miikanissi/modus-themes.nvim]],
    lazy = false
  }
}
