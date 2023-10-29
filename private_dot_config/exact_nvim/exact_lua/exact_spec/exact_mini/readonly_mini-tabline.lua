---@type LazyPluginSpec
return {
  [[echasnovski/mini.tabline]],
  event = [[BufAdd]],

  keys = {
    {
      [[<C-Right>]],
      [[<cmd>bnext<cr>]],
      [[n]],
      desc = [[Tab Next]]
    },
    {
      [[<C-Left>]],
      [[<cmd>bprevious<cr>]],
      [[n]],
      desc = [[Tab Prev]]
    }
  },

  config = true
}
