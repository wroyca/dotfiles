---@type LazyPluginSpec
return {
  [[cmp]],
  event = {
    [[InsertEnter]],
    [[CmdLineEnter]]
  },

  opts = function()
    local cmp = require [[cmp]]
    local cmp_config = require [[cmp.config]]
    local cmp_core = require [[cmp.core]]

    -- nvim_lsp and nvim_lsp_signature_help source still use deprecated
    -- vim.lsp.buf_get_clients(), which is slower due to the deprecation and
    -- version check in that function. Overwrite it using
    -- `vim.lsp.get_clients()` to improve performance.
    --
    function vim.lsp.buf_get_clients(bufnr)
      return vim.lsp.get_clients({ buffer = bufnr })
    end

    -- https://github.com/hrsh7th/nvim-cmp/issues/1797
    --
    local function fast_cmp_visible()
      if not (cmp.core.view and cmp.core.view.custom_entries_view) then
        return false
      end
      return cmp.core.view.custom_entries_view:visible()
    end

    ---@type string?
    local last_key
    vim.on_key(function(k)
      last_key = k
    end)

    ---@type integer
    local last_changed = 0
    local _cmp_on_change = cmp_core.on_change

    ---Spaces/tabs causes higher latency than other keys, e.g. when holding
    ---down 's' the interval between keystrokes is less than 32ms (80
    ---repeats/s keyboard), but when holding spaces/tabs the interval
    ---increases to 100ms, guess is is due ot some other plugins that
    ---triggers on spaces/tabs Spaces/tabs are not useful in triggering
    ---completions in insert mode but can be useful in command-line
    ---autocompletion, so ignore them only when not in command-line mode
    ---
    ---@diagnostic disable-next-line: duplicate-set-field
    ---
    function cmp_core.on_change(self, trigger_event)
      if (last_key == ' ' or last_key == '\t') and string.sub(vim.fn.mode(), 1, 1) ~= 'c' then
        return
      end

      local now = vim.uv.now()
      local fast_typing = now - last_changed < 32
      last_changed = now

      if not fast_typing or trigger_event ~= 'TextChanged' or fast_cmp_visible() then
        _cmp_on_change(self, trigger_event)
        return
      end

      vim.defer_fn(function()
        if last_changed == now then
          _cmp_on_change(self, trigger_event)
        end
      end, 200)
    end

    return {
      sources = cmp.config.sources({
        {
          name = [[buffer]],
          option = {
            -- Faster than the default one because of its syntactical simplificty.
            keyword_pattern = [[\k\+]]
          }
        },
        { name = [[luasnip]] },
        { name = [[nvim_lua]] },

        {
          name = [[nvim_lsp]],
          entry_filter = function(entry)
            -- We provide our own snippets; filter out the ones from LSP.
            --
            return cmp.lsp.CompletionItemKind.Snippet ~= entry:get_kind()
          end
        },
        { name = [[nvim_lsp_signature_help]] },
        { name = [[nvim_lsp_document_symbol]] },
      }),

      cmp.setup.cmdline({ [[/]], [[?]] }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = [[buffer]] }
        }
      }),

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

      mapping = {
        ['<Tab>'] = cmp.mapping(cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Insert,
          select = true
        }), { [[i]] }),

        -- HACK: view.docs.auto_open has a penchant for resetting autonomously,
        -- thanks to some synchronization quirk. Attempts to gracefully close
        -- the doc with cmp.close_docs() are thwarted, as it unfolds too early
        -- in cmp's asynchronous cycle. Now, since we prefer a hands-on
        -- approach to managing open/close actions, the strategy here is to
        -- force it to false on each cycle, and pray that it doesn't tank
        -- performance.
        --
        ['<C-n>'] = cmp.mapping(function(fallback)
          if fast_cmp_visible() then
            cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
            cmp_config.get().view.docs.auto_open = false
          else
            fallback()
          end
        end, { [[i]] }),
        ['<C-p>'] = cmp.mapping(function(fallback)
          if cmp.fast_cmp_visible() then
            cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
            cmp_config.get().view.docs.auto_open = false
          else
            fallback()
          end
        end, { [[i]] }),

        -- Documentation might not always be particularly helpful for
        -- completion, given that many languages have subpar documentation at
        -- best. However, certain languages, such as Rust, make a commendable
        -- effort in this regard. Hence, let's provide a way to toggle and
        -- navigate documentation when necessary.
        --
        ['<A-h>'] = cmp.mapping(function()
          if cmp.visible_docs() then
            cmp.close_docs()
          end
        end, { [[i]] }),
        ['<A-j>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { [[i]], [[c]] }),
        ['<A-k>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { [[i]], [[c]] }),
        ['<A-l>'] = cmp.mapping(function()
          if cmp.visible_docs() ~= true then
            cmp.open_docs()
          end
        end, { [[i]] }),

        -- In real-world scenarios, snippets with more than two nodes usually
        -- aren't very handy. However, there are occasional exceptions, so we
        -- still need proper mapping to deal with those cases.
        --
        ['<A-n>'] = cmp.mapping(function(fallback)
          if require [[luasnip]].jumpable(1) then
            require [[luasnip]].jump(1)
          else
            fallback()
          end
        end, { [[i]] }),
        ['<A-p>'] = cmp.mapping(function(fallback)
          if require [[luasnip]].jumpable(-1) then
            require [[luasnip]].jump(-1)
          else
            fallback()
          end
        end, { [[i]] }),
      },

      formatting = {
        fields = { [[abbr]], [[menu]], [[kind]] },
        format = function(entry, cmp_item)
          pcall(function()
            local detail = entry:get_completion_item().detail
            local client_name = entry.source.source.client.name
            local type_information = {
              [[clangd]]
            }

            -- Include type information from LSP clients that support it. Note
            -- that not every client provides type information for the
            -- completion item. For example, in Python, it can be either nil
            -- or "Auto-import."
            --
            if vim.tbl_contains(type_information, client_name) and detail then
              cmp_item.menu = detail
            end
          end)

          cmp_item.dup  = 0
          cmp_item.abbr = string.gsub(cmp_item.abbr, [[^%s+]], [[]])
          cmp_item.kind = cmp_item.kind .. [[ ]]

          return cmp_item
        end
      }
    }
  end
}
