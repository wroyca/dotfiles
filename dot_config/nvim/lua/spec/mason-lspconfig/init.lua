---@module "mason"

---@type LazyPluginSpec
local Spec = {
 "mason-org/mason-lspconfig.nvim", event = "VeryLazy", opts = {},

  dependencies = {
    "mason.nvim",
    "nvim-lspconfig",
  },
}

return Spec
