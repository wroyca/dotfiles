---@type LazyPluginSpec
return {
  [[hrsh7th/nvim-cmp]],
  event = [[InsertEnter]],
  opts = function()
    local cmp = require [[cmp]]
    local key = require [[detail.key]]
    return {
      --- List completion sources and their priority.
      sources = cmp.config.sources({
        { name = [[nvim_lsp]] },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] },
        { name = [[buffer]] },
        { name = [[luasnip]] },
      }),

      snippet = {
        expand = function(args)
          require [[luasnip]].lsp_expand(args.body)
        end
      },

      window = {
        documentation = false
      },

      performance = {
        max_view_entries = 64
      },

      mapping = key.collector():map({
        {
          [[@cmp.complete]],
          cmp.mapping.complete()
        },
        {
          [[@cmp.confirm_insert]],
          cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert, select = true
          })
        },
        {
          [[@cmp.confirm_replace]],
          cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace, select = true
          })
        },
        {
          [[@cmp.select_prev_item]],
          cmp.mapping.select_prev_item({
            behavior = cmp.SelectBehavior.Select
          })
        },
        {
          [[@cmp.select_next_item]],
          cmp.mapping.select_next_item({
            behavior = cmp.SelectBehavior.Select
          })
        }
      }):collect_lhs_table(),

      formatting = {
        fields = { [[abbr]], [[menu]], [[kind]] },
        format = function(entry, cmp_item)
          -- Retrieve completion detail from LSP if available.
          pcall(function()
            local lsp_item = entry:get_completion_item()
            if lsp_item.detail then
              cmp_item.menu = lsp_item.detail
            end
          end)

          -- Trim leading whitespace (e.g. from clangd)
          cmp_item.abbr = string.gsub(cmp_item.abbr, [[^%s+]], [[]])

          -- Remove duplicates
          cmp_item.dup = 0

          ---@param field string
          ---@param min_width integer
          ---@param max_width integer
          ---@return nil
          local function clamp(field, min_width, max_width)
            if not cmp_item[field] or not type(cmp_item) == [[string]] then
              return
            end
            -- In case that min_width > max_width
            if min_width > max_width then
              min_width, max_width = max_width, min_width
            end
            local field_str = cmp_item[field]
            local field_width = vim.fn.strdisplaywidth(field_str)
            if field_width > max_width then
              local former_width = math.floor(max_width * 0.6)
              local latter_width = math.max(0, max_width - former_width - 1)
              cmp_item[field] = string.format(
                [[%s…%s]],
                field_str:sub(1, former_width),
                field_str:sub(-latter_width)
              )
            elseif field_width < min_width then
              cmp_item[field] = string.format([[%-]] .. min_width .. [[s]], field_str)
            end
          end
          clamp([[abbr]], vim.go.pw, math.max(60, math.ceil(vim.o.columns * 0.4)))
          clamp([[menu]], 0, math.max(16, math.ceil(vim.o.columns * 0.2)))
          return cmp_item
        end
      }
    }
  end
}
