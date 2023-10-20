---@type LazyPluginSpec
return {
  [[hrsh7th/nvim-cmp]],
  event = [[InsertEnter]],

  opts = function()
    local cmp = require [[cmp]]
    local luasnip = require [[luasnip]]
    return {
      --- List completion sources and their priority.
      sources = cmp.config.sources({
        -- Don't search for buffer completion candidates beyond three
        -- characters; they offer the highest accuracy. Transition to LSP
        -- candidates starting from the fourth character.
        { name = [[nvim_lsp]], keyword_length = 4 },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] },
      }, {
        {
          name = [[buffer]],
          priorty = 200,
          option = {
            -- Suggest words from all visible buffers.
            get_bufnrs = function()
              return vim.tbl_map(
                vim.api.nvim_win_get_buf, vim.api.nvim_list_wins()
              )
            end
          }
        },
        { name = [[luasnip]], priority = 100 }
      }),

      mapping = cmp.mapping.preset.insert({
        ['<Tab>'] = cmp.mapping.confirm({ select = true }),
        ['<C-f>'] = cmp.mapping(function(fallback)
          if luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          else
            fallback()
          end
        end, { [[i]], [[s]] }),
        ['<C-b>'] = cmp.mapping(function(fallback)
          if luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { [[i]], [[s]] })
      }),

      snippet = {
        expand = function(args)
          require [[luasnip]].lsp_expand(args.body)
        end
      },

      window = {
        documentation = {
          max_width = 80,
          max_height = 20,
        },
      },

      performance = {
        max_view_entries = 64,
      },

      formatting = {
        fields = { [[abbr]], [[kind]], [[menu]] },
        format = function(entry, cmp_item)
          pcall(function()
            local item = entry:get_completion_item()
            local client = entry.source.source.client.name
            if client == [[clangd]] and item.detail then
              cmp_item.menu = item.detail
            end
          end)

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
      },

      --- Ephemeral-like inline ghost-text.
      ---
      --- Note that starting with nvim 0.10.0 this feature is now considered a bug.
      --- >
      --- > at no point of time has ephemeral inline text (yet) been available
      --- > as an intentional feature. if it was temporarily available it was a
      --- > bug that was fixed.
      --- >
      --- https://github.com/hrsh7th/nvim-cmp/pull/1688
      experimental = {
        ghost_text = {
          hl_group = [[LspCodeLens]]
        }
      }
    }
  end
}
