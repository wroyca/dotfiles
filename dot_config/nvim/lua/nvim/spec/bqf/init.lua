---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-bqf", ft = "qf",

  ---@type BqfConfig
  ---@diagnostic disable: missing-fields
  opts = {
    preview = {
      auto_resize_height = true,
    },
  },
}

return Spec
