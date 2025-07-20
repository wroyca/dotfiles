---@module "mason-conform"

---@type LazyPluginSpec
local Spec = {
  "zapling/mason-conform.nvim", event = "VeryLazy", opts = {},

  dependencies = {
    "mason.nvim",
    "conform.nvim",
  },
}

return Spec
