if vim.g.clangd_c then return end

-- Third-party process are potentially blocking, so await event loop
-- availability.
vim.schedule(function()
  local lsp_config = require [[lspconfig]]
  local lsp_default = lsp_config.clangd.document_config.default_config.cmd[1]
  local lsp_capabilities = vim.lsp.protocol.make_client_capabilities

  -- Prefer Mason, except in cases where the LSP server is locally accessible.
  if vim.fn.executable(lsp_default) == 0 then require [[mason]] end
  if vim.fn.executable(lsp_default) == 1 then
    -- Prefer on_attach for server-specific settings and LspAttach for tightly
    -- coupled modules. Note that "LspAttach" event is sent before lspconfig
    -- setup process.
    vim.api.nvim_create_autocmd([[LspAttach]], {
      callback = function()
        require [[neogen]].setup()
        vim.api.nvim_create_autocmd([[InsertCharPre]], {
          callback = function()
            require [[cmp_nvim_lua]]
            require [[cmp_nvim_lsp_document_symbol]]
            require [[cmp_nvim_lsp_signature_help]]
          end
        })
      end
    })
  end

  lsp_config.clangd.setup {
    capabilities = vim.tbl_deep_extend(
      [[force]],
      {},

      -- Gets new ClientCapabilities object describing LSP client
      -- capabilities.
      lsp_capabilities(),

      -- Provide completion candidates during textDocument/completion
      -- request.
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
      [[--malloc-trim]],
      [[--parse-forwarding-functions]],
      [[--pch-storage=memory]],
      [[--rename-file-limit=0]]
    }
  }

  -- Manual setup as FileType event is behind BufReadPost event
  require [[detail.util]].try_add_wrapper(lsp_config.clangd, [[c]])
end)

vim.g.clangd_c = true
