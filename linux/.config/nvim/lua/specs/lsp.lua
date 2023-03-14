return {
  {
   "neovim/nvim-lspconfig",
    event = {
      "BufReadPre",
      "BufNewFile"
    },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp"
    },
    
    -- I opted not to include any default keybinds in this configuration because I seldom use them. 
    -- Instead, I decided to eliminate them altogether and focus on identifying the few rare ones that I do use.
    --
    config = function()
      require('mason').setup({})
      require('mason-lspconfig').setup({})
      require('mason-lspconfig').setup_handlers({
        function(name) 
          require('lspconfig')[name].setup {capabilities = require('cmp_nvim_lsp').default_capabilities()} 
        end
      })

      local s = {
        Error = '|',
         Warn = '|',
         Hint = '|',
         Info = '|',
      }
      
      for t, i in pairs(s) do
        vim.fn.sign_define('DiagnosticSign' .. t, {text = i, texthl = 'DiagnosticSign' .. t})
      end

      vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {border = 'rounded'})
      vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {border = 'rounded'})
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        signs = true, 
        underline = false,
        update_in_insert = true, 
        virtual_text = false, 
        severity_sort = true
      })
    end
  }
}

