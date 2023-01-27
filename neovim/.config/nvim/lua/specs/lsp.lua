return {
 "neovim/nvim-lspconfig",
  event = "VeryLazy",
  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
  },
  config = function()
    local mason = require('mason')
    local mason_lspconfig = require('mason-lspconfig')

    local opts = { noremap=true, silent=true }

    vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
    local on_attach = function(client, bufnr)
      vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
      local bufopts = { noremap=true, silent=true, buffer=bufnr }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
      vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
      vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
      vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, bufopts)
      vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
      vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
      vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
      vim.keymap.set('n', '<space>;', function() vim.lsp.buf.format { async = true } end, bufopts)
      local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
      function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
        opts = opts or {}
        opts.border = opts.border or borders
        return orig_util_open_floating_preview(contents, syntax, opts, ...)
      end
    end

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
  end,
}

