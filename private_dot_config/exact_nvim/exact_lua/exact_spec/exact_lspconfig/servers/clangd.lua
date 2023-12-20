require [[lspsaga]]
require [[neoconf]]

return function()
  require [[lspconfig]].clangd.setup {
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

    cmd = {
      [[clangd]],
      [[--all-scopes-completion=true]],
      [[--debug-origin=false]],
      [[--background-index=true]],
      [[--background-index-priority=normal]],
      [[--clang-tidy]],
      [[--completion-parse=always]],
      [[--ranking-model=decision_forest]],
      [[--completion-style=bundled]],
      [[--fallback-style=gnu]],
      [[--function-arg-placeholders]],
      [[--header-insertion=never]],
      [[--limit-references=0]],
      [[--limit-results=0]],
      [[--parse-forwarding-functions]],
      [[--pch-storage=memory]],
      [[--rename-file-limit=0]],

      -- Crashes on MacOS and unsupported on Windows.
      --
      vim.fn.has("unix") and [[--malloc-trim]] or nil
    }
  }
end
