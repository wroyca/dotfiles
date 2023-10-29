---@type LazyPluginSpec
return {
  [[neogitorg/neogit]],
  cmd = [[Neogit]],

  dependencies = {
    [[ibhagwan/fzf-lua]],
    [[nvim-lua/plenary.nvim]],
    [[nvim-telescope/telescope.nvim]],
    [[sindrets/diffview.nvim]],
  },

  config = true
}
