---@type LazyPluginSpec
return {
  [[neovim/nvim-lspconfig]],
  event = [[LazyFile]],
  config = function ()
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

    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
      vim.lsp.diagnostic.on_publish_diagnostics,
    {
      signs = false,
      underline = true,
      update_in_insert = false,
      virtual_text = false
    })

    require [[mason-lspconfig]].setup_handlers {
      function(server_name)
        require("lspconfig")[server_name].setup {
          capabilities = vim.tbl_deep_extend(
            [[force]],
            {},
            vim.lsp.protocol.make_client_capabilities(),
            require [[cmp_nvim_lsp]].default_capabilities()
          )
        }
      end,
      ["clangd"] = require [[spec.lspconfig.servers.clangd]],
      ["lua_ls"] = require [[spec.lspconfig.servers.lua_ls]]
    }
  end
}
