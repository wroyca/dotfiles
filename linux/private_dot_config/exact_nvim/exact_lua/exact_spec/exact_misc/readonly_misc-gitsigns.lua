return {
  [[misc-gitsigns]],
  event = [[VeryLazy]],
  cond = function()
    return vim
      .system({
        [[git]],
        [[rev-parse]],
        [[--is-inside-work-tree]],
      }, { cwd = vim.fn.getcwd() })
      :wait().stdout == "true\n"
  end,
  opts = {
    signs = {
      add = {
        text = [[┃]],
      },
      untracked = {
        text = [[ ]],
      },
      change = {
        text = [[┃]],
      },
      delete = {
        text = [[┃]],
      },
      topdelete = {
        text = [[┃]],
      },
      changedelete = {
        text = [[┃]],
      },
    },
  },
}
