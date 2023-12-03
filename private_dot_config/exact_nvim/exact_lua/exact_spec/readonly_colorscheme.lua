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
        config = function()
          local MiniColors = require [[mini.colors]]
          local is_first_run = true
          vim.api.nvim_create_autocmd(
            [[OptionSet]],
          {
            pattern = [[background]],
            callback = function()
              local cs = { MiniColors.get_colorscheme() or {} }
              vim.o.background = (vim.o.background == [[light]]) and [[dark]] or [[light]]
              if is_first_run ~= true then
                MiniColors.animate(cs, {
                  transition_steps = 100
                })
              else
                MiniColors.animate(cs, {
                  transition_duration = 0
                })
              end
              is_first_run = false
            end
          })
        end
      }
    },

    ---@diagnostic disable-next-line: assign-type-mismatch
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
