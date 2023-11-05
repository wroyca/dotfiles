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
  end
}
