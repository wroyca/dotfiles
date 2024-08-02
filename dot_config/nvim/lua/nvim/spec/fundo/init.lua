---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", event = "VeryLazy", dependencies = "kevinhwang91/promise-async",

  init = function ()
    vim.o.undofile = true
  end,

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },
}

return Spec
