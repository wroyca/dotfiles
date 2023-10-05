---@type LazyPluginSpec
return {
  [[misc-visual-multi]],
  keys = {
    [[<C-n>]],
    [[<C-N>]],
    [[<M-n>]],
    [[<S-Down>]],
    [[<S-Up>]],
    [[<M-Left>]],
    [[<M-i>]],
    [[<M-Right>]],
    [[<M-D>]],
    [[<M-Down>]],
    [[<C-d>]],
    [[<C-Down>]],
    [[<C-Up>]],
    [[<S-Right>]],
    [[<C-LeftMouse>]],
    [[<M-LeftMouse>]],
    [[<M-C-RightMouse>]],
  },
  config = function()
    vim.g.VM_silent_exit = 1
    vim.g.VM_show_warnings = 0
  end,
}
