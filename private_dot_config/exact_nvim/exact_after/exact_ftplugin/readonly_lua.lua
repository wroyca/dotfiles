if vim.g.lua_ls then
  return
end

-- Initiating third-party process is a potentially blocking task, so await
-- event loop availability.
--
vim.schedule(function()
  local lsp_config = require [[lspconfig]]
  local lsp_default = lsp_config.lua_ls.document_config.default_config.cmd[1]
  local lsp_capabilities = vim.lsp.protocol.make_client_capabilities

  -- Prefer Mason, except in cases where a local version is accessible.
  --
  if vim.fn.executable(lsp_default) == 0 then require [[mason]] end
  if vim.fn.executable(lsp_default) == 1 then
    -- Prefer on_attach for configuring server-specific settings, and
    -- LspAttach to import tightly coupled modules.
    --
    vim.api.nvim_create_autocmd([[LspAttach]], {
      callback = function()
        require [[neodev]].setup()
        vim.api.nvim_create_autocmd([[InsertCharPre]], {
          callback = function()
            require [[neogen]].setup({})
            require [[cmp_nvim_lua]]
            require [[cmp_nvim_lsp_document_symbol]]
            require [[cmp_nvim_lsp_signature_help]]
          end
        })
      end
    })
  end

  lsp_config.lua_ls.setup {
    capabilities = vim.tbl_deep_extend(
      [[force]],
      {},

      -- Gets new ClientCapabilities object describing LSP client
      -- capabilities.
      --
      lsp_capabilities(),

      -- Provide completion candidates during textDocument/completion
      -- request.
      --
      require [[cmp_nvim_lsp]].default_capabilities()
    ),

    on_attach = function(client, buffer)
      require [[detail.util]].try_add_inlay(client, buffer)
    end
  }
  require [[detail.util]].try_add_wrapper(lsp_config.lua_ls, [[lua]])
end)

vim.g.lua_ls = true
