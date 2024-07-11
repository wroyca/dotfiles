---@type LazyPluginSpec
return {
  [[mini.tabline]], event = [[VeryLazy]],

  dependencies = {
    {
      [[tiagovla/scope.nvim]],
      config = true
    }
  },

  keys = {
    { [[<leader>bn]], vim.cmd.bnext,        desc = [[Next]]     },
    { [[<leader>bp]], vim.cmd.bprev,        desc = [[Previous]] },
    { [[<leader>btN]], vim.cmd.tabnew,      desc = [[New]]      },
    { [[<leader>btD]], vim.cmd.tabclose,    desc = [[Delete]]   },
    { [[<leader>btn]], vim.cmd.tabnext,     desc = [[Next]]     },
    { [[<leader>btp]], vim.cmd.tabprevious, desc = [[Previous]] },
  },

  opts = {
    tabpage_section = [[right]]
  }
}
