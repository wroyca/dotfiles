---@module "bqf"
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-bqf", ft = "qf",

  ---@type BqfConfig
  opts = {
    preview = {
      auto_resize_height = true,
    },
  },
}

return Spec
