---@type LazyPluginSpec
return {
  [[kevinhwang91/nvim-fundo]],
  lazy = false,
  build = function()
    require [[fundo]].install()
  end,

  opts = {
    limit_archives_size = 9999
  },

  -- BUG: Normally, lazy.nvim handles this automatically, but with fundo, it
  -- doesn't work as expected unless we explicitly provide 'opts' during
  -- setup.
  config = function(_, opts)
    require [[fundo]].setup(opts)
  end
}
