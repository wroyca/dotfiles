return {
 "neovim/nvim-lspconfig",
  event = "InsertEnter",
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
  },
  config = function()
    local mason = require('mason')
    local mason_lspconfig = require('mason-lspconfig')

    mason.setup({
      ui = {
        icons = {
          package_installed   = "|",
          package_pending     = "|",
          package_uninstalled = "|"
        }
      }
    })

    mason_lspconfig.setup({
      ensure_installed = {
        "clangd"
      },
      automatic_installation = true
    })

    mason_lspconfig.setup_handlers({
      function(name)
        local handler = require('lspconfig')[name]
        handler.setup {
          on_attach = on_attach,
          capabilities = require('cmp_nvim_lsp').default_capabilities()
        }
      end,
    })

    local signs = {
      Error = "|",
      Warn  = "|",
      Hint  = "|",
      Info  = "|"
    }

    for type, icon in pairs(signs) do
      local sign = "DiagnosticSign" .. type
      vim.fn.sign_define(sign, {
        text = icon, texthl = "DiagnosticSign" .. type, numhl = "DiagnosticSign" .. type
      })
    end

    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = "rounded"
    })

    vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
      border = "rounded"
    })

    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      float = {
        source = 'always',
        header = '',
        prefix = ''
      },
      signs = true,
      underline = true,
      update_in_insert = true,
      virtual_text = true
    })
  end
}

