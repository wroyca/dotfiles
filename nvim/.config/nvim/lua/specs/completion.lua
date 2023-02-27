return {
  {
   "hrsh7th/nvim-cmp",
    version = false,
    dependencies = 
    {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lsp-document-symbol",
      -- Among the various choices, I have discovered that "hrsh7th/cmp-vsnip" 
      -- and "hrsh7th/vim-vsnip" yield the most reliable outcomes. Conversely, 
      -- "saadparwaiz1/cmp_luasnip" has superior snippets, but its suggestion 
      -- mechanism is overly forceful, prompting snippets instead of 
      -- source-based completions when the latter is desired.
      -- 
      "hrsh7th/cmp-vsnip",
      "hrsh7th/vim-vsnip",
    },
    config = function()
      local cmp = require('cmp')
      cmp.setup({
        formatting = {
          format = require('lspkind').cmp_format({
            mode = 'symbol_text',
            maxwidth = 50,
            ellipsis_char = '...',
            before = function (entry, vim_item)
              return vim_item
            end
          })
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp.mapping( function(fallback) if cmp.visible() then cmp.select_next_item() else fallback() end end, {"i", "s"}),
          ["<S-Tab>"] = cmp.mapping( function(fallback) if cmp.visible() then cmp.select_prev_item() else fallback() end end, {"i", "s"}),
        }),
        snippet = {
          expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
          end
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'nvim_lsp_signature_help' },
          { name = 'nvim_lua' },
          { name = 'vsnip' },
          { name = 'buffer' },
        }),
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        -- Despite its overall appeal, ghost_text is rendered impractical in 
        -- nvim due to its "normal" highlight, which encompasses most "normal" 
        -- text such as cmp, doc, and others.
        --
        experimental = {
          ghost_text = {
            -- hl_group = "LspCodeLens",
          },
        },
      })
      cmp.setup.filetype('gitcommit', {
        sources = cmp.config.sources({{ name = 'cmp_git' }}, {{ name = 'buffer' }})
      })
      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {{ name = 'nvim_lsp_document_symbol' }, { name = 'buffer' }}
      })
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({{ name = 'path' }}, {{ name = 'cmdline' }})
      })
    end
  }
}

