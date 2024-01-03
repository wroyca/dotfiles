---@type LazyPluginSpec
return {
  [[neogitorg/neogit]],
  dependencies = {
    [[ibhagwan/fzf-lua]],
    [[sindrets/diffview.nvim]],
  },

  keys = {
    { [[<leader>gn]], [[<cmd>Neogit<cr>]], desc = [[Neogit]] },
  },

  config = true
}
