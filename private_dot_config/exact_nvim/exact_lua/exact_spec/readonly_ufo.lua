---@type LazyPluginSpec
return {
  [[kevinhwang91/nvim-ufo]],
  event = [[VeryLazy]],
  opts = {
    provider_selector = function(_, _, _)
      -- NOTE: LSP folding range requests are very slow, whereas treesitter
      -- is instantaneous, even with larger files.
      return { [[treesitter]], [[indent]] }
    end
  }
}
