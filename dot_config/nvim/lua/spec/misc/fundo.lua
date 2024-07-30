---@type LazyPluginSpec
local Spec = {
  [[fundo]], event = [[VeryLazy]], dependencies = [[promise-async]],

  init = function ()
    vim.o.undofile = true
  end,

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },
}

return Spec
