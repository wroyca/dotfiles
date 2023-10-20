---@type LazyPluginSpec
return {
  [[williamboman/mason-lspconfig.nvim]],
  opts = {
    ensure_installed = { [[lua_ls]] },
    automatic_installation = true
  }
}
