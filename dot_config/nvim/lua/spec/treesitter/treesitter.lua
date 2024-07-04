---@type LazySpec
return {
  [[treesitter]], event = [[VeryLazy]],

  main = [[nvim-treesitter.configs]],
  opts = {
    ensured_installed = { [[all]] },
    highlight = {
      enable = true
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        node_incremental = [[v]],
        node_decremental = [[V]]
      }
    }
  }
}
