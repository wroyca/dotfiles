---@type LazyPluginSpec
return {
  [[kevinhwang91/nvim-ufo]],
  name = [[ufo]],
  event = [[BufReadPost]],

  opts = {
    provider_selector = function(_, _, _)
      return {
        [[treesitter]],
        [[indent]]
      }
    end
  }
}
