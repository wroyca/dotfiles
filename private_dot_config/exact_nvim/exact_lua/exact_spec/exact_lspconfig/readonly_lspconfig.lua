---@type LazyPluginSpec
return {
  [[neovim/nvim-lspconfig]],
  name = [[lspconfig]],
  config = function()
    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
      vim.lsp.diagnostic.on_publish_diagnostics,
    {
      signs = false,
      underline = true,
      update_in_insert = false,
      virtual_text = false
    })
  end
}
