---@type LazyPluginSpec
return {
  [[kevinhwang91/nvim-fundo]],
  name = [[fundo]],
  event = [[BufReadPost]],

  opts = {
    limit_archives_size = 9999
  },

  build = function()
    require [[fundo]].install()
  end,

  -- BUG: Lazy.nvim doesn't automatically pass opts for this plugin.
  config = function(_, opts)
    require [[fundo]].setup(opts)
  end
}
