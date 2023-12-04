---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[f-person/auto-dark-mode.nvim]],
    name = [[colorscheme]],
    event = [[VeryLazy]],
    dependencies = {
      {
        [[echasnovski/mini.colors]],
        config = true
      }
    },

    ---@diagnostic disable-next-line: assign-type-mismatch
    config = {
      set_dark_mode = function()
        local MiniColors = require [[mini.colors]]
        local cs = { MiniColors.get_colorscheme([[dark]]) }

        MiniColors.animate(cs, {
          transition_steps = 100
        })
      end,

      set_light_mode = function()
        local MiniColors = require [[mini.colors]]
        local cs = { MiniColors.get_colorscheme([[light]]) }

        MiniColors.animate(cs, {
          transition_steps = 100
        })
      end
    }
  }
}
