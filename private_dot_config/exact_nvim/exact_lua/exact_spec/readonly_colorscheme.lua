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

  -- https://github.com/shaun-mathew/Chameleon.nvim/pull/3
  {
    [[wroyca/Chameleon.nvim]],
    name = [[chameleon]],
    event = [[VeryLazy]],
    cond = vim.fn.expand('$TERM') == [[xterm-kitty]] and not vim.g.neovide,
    config = true
  },

  -- Every now and then, I might desire a change of pace.
  {
    [[miikanissi/modus-themes.nvim]],
    lazy = false
  }
}
