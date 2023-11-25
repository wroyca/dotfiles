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

    return {
      --- List completion sources and their priority.
      sources = cmp.config.sources({
        { name = [[nvim_lsp]] },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] },

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
          vim.snippet.expand(args.body)
        end
      },

      mapping = key.collector():map(
        {
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
            local cmp_item = entry:get_completion_item()
            if entry.source.source.client.name == [[clangd]] and cmp_item.detail then
              vim_item.menu = cmp_item.detail
            end
          end)

          local item_abbr = vim_item.abbr
          local win_width = vim.api.nvim_win_get_width(0)
          local pum_width = 40
          local max_width = pum_width - 10 or math.floor(win_width * 0.2)

          if #item_abbr > max_width then
            vim_item.abbr = vim.fn.strcharpart(item_abbr, 0, max_width - 3) .. "..."
          else
            vim_item.abbr = item_abbr .. (" "):rep(max_width - #item_abbr)
          end

          vim.o.pumwidth = pum_width
          return vim_item
        end
      }
    }
  end
}
