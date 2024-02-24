return {
  [[misc-gitsigns]],
  event = [[VeryLazy]],
  cond = function()
    local cmd = { [[git]], [[rev-parse]], [[--is-inside-work-tree]] }
    return vim.system(cmd, {
      cwd = vim.fn.getcwd()
    }):wait().stdout == "true\n"
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
