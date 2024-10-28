---@module "mason-lspconfig"

---@type LazyPluginSpec
local Spec = {
  "williamboman/mason-lspconfig.nvim", event = "User AsyncFileLoad", priority = 10,

  opts = {
    automatic_installation = true,
  }
}

return Spec
