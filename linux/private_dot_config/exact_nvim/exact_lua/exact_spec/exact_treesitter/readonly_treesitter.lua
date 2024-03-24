---@type LazySpec
return {
  [[treesitter]],
  cmd = [[TSInstall]],
  init = function(plugin)
    require [[lazy.core.loader]].add_to_rtp(plugin)
    require [[nvim-treesitter.query_predicates]]
  end,
  main = [[nvim-treesitter.configs]],
  opts = { indent = { enable = false }, highlight = { enable = false } }
}
