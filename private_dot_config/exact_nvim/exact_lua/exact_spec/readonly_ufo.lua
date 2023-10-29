---@type LazyPluginSpec
return {
  [[kevinhwang91/nvim-ufo]],
  name = [[ufo]],
  event = [[BufReadPost]],

  opts = {
    provider_selector = function(_, _, _)
      -- PERF: Prefer treesiter over LSP's folding range requests.
      --
      return {
        [[treesitter]],
        [[indent]]
      }
    end
  }
}
