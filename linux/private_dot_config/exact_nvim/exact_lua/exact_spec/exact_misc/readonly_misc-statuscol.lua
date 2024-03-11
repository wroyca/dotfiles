---@type LazyPluginSpec
return {
  [[misc-statuscol]],
  event = {
    [[BufReadPre]],
    [[BufNewFile]]
  },

  opts = function()
    local builtin = require [[statuscol.builtin]]
    return {
      bt_ignore = { [[nofile]], [[prompt]], [[terminal]], [[packer]]  },
      ft_ignore = {
        [[NeogitCommitMessage]],
        [[NeogitPopup]],
        [[NeogitStatus]],
        [[dap-repl]],
        [[dapui_breakpoints]],
        [[dapui_console]],
        [[dapui_scopes]],
        [[dapui_stacks]],
        [[dapui_watches]],
        [[help]],
        [[lazy]],
        [[man]],
        [[neotest-summary]],
        [[starter]],
        [[toggleterm]]
      },
      segments = {
        {
          sign = { namespace = { [[gitsigns]] }, colwidth = 2, wrap = true },
          click = [[v:lua.ScSa]]
        },
        {
          text = { builtin.lnumfunc, [[ ]] },
          condition = { true, builtin.not_empty },
        },
        {
          text = { [[ ]] }, hl = [[Normal]]
        }
      }
    }
  end
}
