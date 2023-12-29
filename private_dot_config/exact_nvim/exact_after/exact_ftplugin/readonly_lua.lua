if vim.g.lua then return end

vim.schedule(function()
  local lsp_config = require [[lspconfig]]
  local lsp_default = lsp_config.lua_ls.document_config.default_config.cmd[1]
  local lsp_capabilities = vim.lsp.protocol.make_client_capabilities

  if vim.fn.executable(lsp_default) == 0 then require [[mason]] end
  if vim.fn.executable(lsp_default) == 1 then
    require [[neodev]]
    require [[neoconf]]
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
  end -- lspconfig has its own error propagation should it fail.

  lsp_config.lua_ls.setup {
    capabilities = vim.tbl_deep_extend(
      [[force]],
      {},
      lsp_capabilities(),
      require [[cmp_nvim_lsp]].default_capabilities()
    ),

    settings = {
      Lua = {
        workspace = {
          library = vim.api.nvim_get_runtime_file([[]], true),
          checkThirdParty = false,
        }
      }
    }
  }

  for _, buf in pairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local ftype = vim.api.nvim_get_option_value([[filetype]], { buf = buf })
      for _, t in ipairs({[[lua]]}) do
        if ftype == t then
          lsp_config.lua_ls.manager:try_add_wrapper(buf)
          break
        end
      end
    end
  end
end)

vim.g.lua = true
