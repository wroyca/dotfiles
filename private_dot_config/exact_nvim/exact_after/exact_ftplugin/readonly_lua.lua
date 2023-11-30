if vim.g.lua_ls then return end

-- Third-party process are potentially blocking, so await event loop
-- availability.
--
vim.schedule(function()
  local lsp_config = require [[lspconfig]]
  local lsp_default = lsp_config.lua_ls.document_config.default_config.cmd[1]
  local lsp_capabilities = vim.lsp.protocol.make_client_capabilities

  -- Prefer Mason, unless LSP server is provided by the
  -- system package manager.
  --
  if vim.fn.executable(lsp_default) == 0 then require [[mason]] end
  if vim.fn.executable(lsp_default) == 1 then
    -- Load context-sensitive modules prior to any LSP event.
    --
    require [[lspsaga]]
    require [[neodev]].setup()
    require [[neoconf]].setup()

    -- Prefer on_attach for server-specific settings and LspAttach for tightly
    -- coupled modules.
    --
    vim.api.nvim_create_autocmd([[LspAttach]], {
      once = true,
      callback = function()
        vim.api.nvim_create_autocmd([[InsertCharPre]], {
          once = true,
          callback = function()
            require [[cmp_nvim_lua]]
            require [[cmp_nvim_lsp_document_symbol]]
            require [[cmp_nvim_lsp_signature_help]]
          end
        })
      end
    })
  end -- lspconfig has its own error propagation should it fail

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

    settings = {
      Lua = {
        completion = {
          callSnippet = [[Replace]]
        },
        workspace = {
          library = vim.api.nvim_get_runtime_file([[]], true),
          checkThirdParty = false,
        }
      }
    }
  }

  -- Try to attach to available buffers as nvim's FileType event is behind
  -- BufReadPost
  --
  require [[detail.util]].try_add_wrapper(lsp_config.lua_ls, [[lua]])
end)

vim.g.lua_ls = true
