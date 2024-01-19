---@type LazyPluginSpec
return {
  [[misc-fundo]],

  build = function()
    require [[fundo]].install()
  end,

  opts = {
    limit_archives_size = 9999
  },

  -- BUG: Shouldn't be necessary, but something breaks unless we call setup manually.
  --
  config = function(_, opts)
    require [[fundo]].setup(opts)
  end
}
