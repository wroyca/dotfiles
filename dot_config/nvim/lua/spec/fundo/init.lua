---@module "fundo"
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", dependencies = "kevinhwang91/promise-async", event = "VeryLazy",

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },
}

return Spec
