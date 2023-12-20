require [[lspsaga]]
require [[neodev]]
require [[neoconf]]

return function()
  require [[lspconfig]].lua_ls.setup {
    capabilities = vim.tbl_deep_extend(
      [[force]],
      {},

      -- Gets new ClientCapabilities object describing LSP client
      -- capabilities.
      --
      vim.lsp.protocol.make_client_capabilities(),

      -- Provide completion candidates during textDocument/completion
      -- request.
      --
      require [[cmp_nvim_lsp]].default_capabilities()
    ),

    settings = {
      Lua = {
        completion = {
          callSnippet = [[Replace]]
        },
        workspace = {
          library = vim.api.nvim_get_runtime_file([[]], true)
        }
      }
    }
  }
end
