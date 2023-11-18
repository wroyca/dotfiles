---@type LazyPluginSpec
return {
  [[f-person/auto-dark-mode.nvim]],
  lazy = false,
  priority = 1000,

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
}
