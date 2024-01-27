if vim.g.clangd then return end

vim.schedule(function()
  local lsp_config = require [[lspconfig]]
  local lsp_default = lsp_config.clangd.document_config.default_config.cmd[1]
  local lsp_capabilities = vim.lsp.protocol.make_client_capabilities

  if vim.fn.executable(lsp_default) == 0 then require [[mason]] end
  if vim.fn.executable(lsp_default) == 1 then
    vim.api.nvim_create_autocmd([[LspAttach]], {
      once = true,
      callback = function()
        vim.api.nvim_create_autocmd([[InsertCharPre]], {
          once = true,
          callback = function()
            require [[cmp_nvim_lsp_document_symbol]]
            require [[cmp_nvim_lsp_signature_help]]
          end
        })
      end
    })
  end -- lspconfig has its own error propagation should it fail.

  lsp_config.clangd.setup {
    capabilities = vim.tbl_deep_extend(
      [[force]],
      {},
      lsp_capabilities(),
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

      vim.fn.has([[unix]]) and [[--malloc-trim]] or nil
    }
  }

  for _, buf in pairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local ftype = vim.api.nvim_get_option_value([[filetype]], { buf = buf })
      for _, t in ipairs({[[cpp]], [[c]]}) do
        if ftype == t then
          lsp_config.clangd.manager:try_add_wrapper(buf)
          break
        end
      end
    end
  end
end)

vim.g.clangd = true
