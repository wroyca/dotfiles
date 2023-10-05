---@type LazyPluginSpec
return {
  [[misc-fundo]],
  event = [[VeryLazy]],
  build = function()
    require([[fundo]]).install()
  end,
  opts = {
    limit_archives_size = 9999,
  },
}
