---@type LazySpec
return {
  [[treesitter]], event = [[VeryLazy]],

  main = [[nvim-treesitter.configs]],
  opts = {
    incremental_selection = {
      enable = true,
      keymaps = {
        node_incremental = [[v]],
        node_decremental = [[V]]
      }
    }
  }
}
