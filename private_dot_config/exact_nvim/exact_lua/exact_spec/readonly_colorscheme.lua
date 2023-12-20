---@diagnostic disable: assign-type-mismatch
---@type LazyPluginSpec
return {
  {
    [[raddari/last-color.nvim]],
    name = [[colorscheme-last-color]],
    lazy = false,
    priority = 1000,
    config = function()
      vim.schedule(function()
        pcall(vim.api.nvim_exec2, (([[colorscheme %s]]):format(
          require([[last-color]]).recall()
        )), {})
      end)
    end
  },

  {
    [[f-person/auto-dark-mode.nvim]],
    name = [[colorscheme]],
    event = [[VeryLazy]],

    config = {
      set_dark_mode = function()
        vim.o.background = [[dark]]
      end,
      set_light_mode = function()
        vim.o.background = [[light]]
      end
    }
  }
}
