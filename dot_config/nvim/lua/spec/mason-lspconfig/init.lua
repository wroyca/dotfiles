---@module "mason-lspconfig"

---@type LazyPluginSpec
local Spec = {
  "mason-org/mason-lspconfig.nvim", dependencies = "mason.nvim", event = "User LazyFile",

  opts = {
    ensure_installed = {
      "lua_ls",
      "clangd"
    },
  },
}

return Spec
