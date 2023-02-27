return {
  {
   "neovim/nvim-lspconfig",
    event =
    {
      "BufReadPre",
      "BufNewFile"
    },
    dependencies =
    {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim"
    },
    config = function()
      local lspconfig = require('lspconfig')
      local mason = require('mason')
      local mason_lspconfig = require('mason-lspconfig')

      -- With the aid of a compilation database, LSP is capable of identifying
      -- project files and extracting imperative compiler information, such as
      -- include paths and compilation flags. It's worth noting that if a
      -- compilation database cannot be located in the root directory, LSP will
      -- be disabled by default.
      --
      lspconfig.clangd.setup({
        root_dir = lspconfig.util.root_pattern('compile_commands.json'),
      })

      local function with_desc(opts, desc)
        return vim.tbl_extend('force', opts, { desc = desc })
      end

      local opts = { noremap = true, silent = true }
      vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, with_desc(opts, 'Open diagnostic float'))
      vim.keymap.set('n', '[e', vim.diagnostic.goto_prev, with_desc(opts, 'Goto prev diagnostic'))
      vim.keymap.set('n', ']e', vim.diagnostic.goto_next, with_desc(opts, 'Goto next diagnostic'))
      vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, with_desc(opts, 'Open diagnastic loclist'))

      local on_attach = function(_, bufnr)
        -- Mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local bufopts = { noremap = true, silent = true, buffer = bufnr }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, with_desc(bufopts, 'Goto declaration'))
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, with_desc(bufopts, 'Goto definition'))
        vim.keymap.set('n', 'gdd', vim.lsp.buf.type_definition, with_desc(bufopts, 'Goto type definition'))
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, with_desc(bufopts, 'Goto implementation'))
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, with_desc(bufopts, 'Goto references'))
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, with_desc(bufopts, 'Show documentation'))
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, with_desc(bufopts, 'Show signature help'))
        vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, with_desc(bufopts, 'Add workspace folder'))
        vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, with_desc(bufopts, 'Remove workspace folder'))
        vim.keymap.set('n', '<leader>wl', function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, with_desc(bufopts, 'List workspace folders'))
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, with_desc(bufopts, 'Rename'))
        vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, with_desc(bufopts, 'Code actions'))
        vim.keymap.set('n', '<leader>;', function() vim.lsp.buf.format { async = true } end, with_desc(bufopts, 'Format'))
      end

      mason.setup({
        ui = {
          icons = {
            package_installed = "",
            package_pending = "",
            package_uninstalled = ""
          }
        },
      })

      mason_lspconfig.setup({
        ensure_installed = {
          "clangd",
        },
        automatic_installation = true
      })

      mason_lspconfig.setup_handlers({
        function(name)
         local handler = lspconfig[name]
         handler.setup {
           on_attach = on_attach,
           capabilities = capabilities
         }
        end
      })

      local signs = { Error = '', Warn = '', Hint = 'ﴞ', Info = '' }
      for type, icon in pairs(signs) do
        vim.fn.sign_define('DiagnosticSign' .. type, { text = icon, texthl = 'DiagnosticSign' .. type })
      end

      vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = 'rounded',
      })

      vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
        border = 'rounded',
      })

      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        signs = true, underline = true, update_in_insert = false, virtual_text = { spacing = 4, prefix = "●" },severity_sort = true,
      })
    end
  }
}

