local key = require [[detail.key]]

---@type LazyPluginSpec
return {
  [[hrsh7th/nvim-cmp]],
  name = [[cmp]],
  event = { [[InsertEnter]], [[CmdlineEnter]] },

  opts = function()
    local cmp = require [[cmp]]

    local function confirm(behavior)
      return cmp.mapping.confirm({
        behavior = behavior,
        select = true
      })
    end

    local luasnip_next = cmp.mapping(function(fallback)
      local luasnip = require [[luasnip]]
      if luasnip.locally_jumpable(1) then
        luasnip.jump(1)
      end
    end)

    local luasnip_previous = cmp.mapping(function(fallback)
      local luasnip = require [[luasnip]]
      if luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      end
    end)

    return {
      --- List completion sources and their priority.
      sources = cmp.config.sources({
        { name = [[nvim_lsp]] },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] },

        { name = [[luasnip]] },
        { name = [[buffer]] }
      }),

      -- Set configuration for specific filetype.
      cmp.setup.filetype([[gitcommit]], {
        sources = cmp.config.sources({
          { name = [[git]] },
          { name = [[conventionalcommits]] }
        }, {
          { name = [[buffer]] },
        })
      }),

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({ [[/]], [[?]] }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = [[buffer]] }
        }
      }),

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline([[:]], {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = [[path]] }
        }, {
          { name = [[cmdline]] }
        })
      }),

      snippet = {
        expand = function(args)
          require [[luasnip]].lsp_expand(args.body)
        end
      },

      mapping = key.collector():map(
        {
          { [[@cmp.snip_next]],        luasnip_next, },
          { [[@cmp.snip_previous]],    luasnip_previous, },
          { [[@cmp.abort]],            cmp.mapping.abort() },
          { [[@cmp.complete]],         cmp.mapping.complete() },
          { [[@cmp.confirm_insert]],   confirm(cmp.ConfirmBehavior.Insert) },
          { [[@cmp.confirm_replace]],  confirm(cmp.ConfirmBehavior.Replace) },
          { [[@cmp.select_prev_item]], cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }) },
          { [[@cmp.select_next_item]], cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }) },
          { [[@cmp.scroll_docs_up]],   cmp.mapping.scroll_docs(-4) },
          { [[@cmp.scroll_docs_down]], cmp.mapping.scroll_docs(4) },
        }
      ):collect_lhs_table(),


      formatting = {
        fields = {
          [[abbr]],
          [[kind]],
          [[menu]]
        },

        format = function(entry, vim_item)
          pcall(function()
            local completion_item = entry:get_completion_item() --[[@as lsp.CompletionItem]]
            if completion_item.detail then
              vim_item.detail = completion_item.detail
            end
          end)
          return vim_item
        end
      }
    }
  end
}
