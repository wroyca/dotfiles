---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
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

  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[f-person/auto-dark-mode.nvim]],
    name = [[colorscheme]],
    event = [[VeryLazy]],

    ---@diagnostic disable-next-line: assign-type-mismatch
    config = {
      set_dark_mode = function()
        vim.o.background = [[dark]]
      end,
      set_light_mode = function()
        vim.o.background = [[light]]
      end
    }
  },

  { [[folke/tokyonight.nvim]], name = [[colorscheme-tokyonight]] },
  { [[catppuccin/nvim]],       name = [[colorscheme-catppuccin]] }
}
