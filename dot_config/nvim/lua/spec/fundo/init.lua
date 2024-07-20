return {
  [[kevinhwang91/nvim-fundo]], name = [[fundo]], event = [[VeryLazy]],

  build = function()
    require [[fundo]].install()
  end,

  init = function()
    vim.o.undofile = true
  end,

  opts = {
    limit_archives_size = 9999
  }
}
