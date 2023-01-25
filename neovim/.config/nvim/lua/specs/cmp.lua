return {
 "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = { 
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-vsnip",
    "hrsh7th/vim-vsnip",
  },
  config = function()
    local cmp = require("cmp")
    local borders = {
      { '╭', 'FloatBorder' }, 
      { '─', 'FloatBorder' }, 
      { '╮', 'FloatBorder' }, 
      { '│', 'FloatBorder' }, 
      { '╯', 'FloatBorder' }, 
      { '─', 'FloatBorder' }, 
      { '╰', 'FloatBorder' }, 
      { '│', 'FloatBorder' }
    }
    
    cmp.setup({
      formatting = { 
        format = require("lspkind").cmp_format({
          mode = 'symbol',
          maxwidth = 50,
          ellipsis_char = '...',
          before = function (entry, vim_item)
            return vim_item
          end
        })
      },
      snippet = {
        expand = function(args)
          vim.fn["vsnip#anonymous"](args.body)
        end,
      },
      window = {
        completion = {
          border = borders,
          winhighlight = 'NormalFloat:CmpDocumentation,FloatBorder:CmpDocumentationBorder',
        },
        documentation = {
          border = borders,
          winhighlight = 'NormalFloat:CmpDocumentation,FloatBorder:CmpDocumentationBorder',
        }
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<Tab>"] = cmp.mapping( function(fallback) if cmp.visible() then cmp.select_next_item() else fallback() end end, {"i", "s"}),
        ["<S-Tab>"] = cmp.mapping( function(fallback) if cmp.visible() then cmp.select_prev_item() else fallback() end end, {"i", "s"}),
      }),
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'vsnip' },
      }, {
        { name = 'buffer' },
      })
    })

    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = 'buffer' }
      }
    })

    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = 'path' }}, {{ name = 'cmdline' }
      })
    })
  end
}

