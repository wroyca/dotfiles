local fd_opts = {
  fd_cmd = { 'fd', '-p', '-H', '-L', '-td', '-tf', '-tl', '-d4', '--mount', '-c=never',
  '-E=*$*',         '-E=*%*',                   '-E=*.bkp',     '-E=*.bz2',     '-E=*.db',     '-E=*.jar',
  '-E=*.directory', '-E=*.dll',                 '-E=*.doc',     '-E=*.docx',    '-E=*.drawio', '-E=*.otf',
  '-E=*.gif',       '-E=*.git/',                '-E=*.gz',      '-E=*.ico',     '-E=*.iso',    '-E=*.pptx',
  '-E=*.jpeg',      '-E=*.jpg',                 '-E=*.mp3',     '-E=*.mp4',     '-E=*.o',      '-E=*.ttf',
  '-E=*.out',       '-E=*.pdf',                 '-E=*.pickle',  '-E=*.png',     '-E=*.ppt',    '-E=*\\~',
  '-E=*.pyc',       '-E=*.rar',                 '-E=*.so',      '-E=*.svg',     '-E=*.tar',
  '-E=*.venv/',     '-E=*.xls',                 '-E=*.xlsx',    '-E=*.zip',     '-E=*Cache*/',
  '-E=*cache*/',    '-E=.*Cache*/',             '-E=.*cache*/', '-E=.*wine/',   '-E=.cargo/',
  '-E=.conda/',     '-E=.dot/',                 '-E=.fonts/',   '-E=.ipython/', '-E=.java/',
  '-E=.jupyter/',   '-E=.luarocks/',            '-E=.mozilla/', '-E=.npm/',     '-E=.nvm/',
  '-E=.steam*/',    '-E=.thunderbird/',         '-E=.tmp/',     '-E=__pycache__/',
  '-E=dosdevices/', '-E=events.out.tfevents.*', '-E=node_modules/',
  '-E=vendor/',     '-E=venv/'
  }
}

---@diagnostic disable: undefined-field
---@type LazyPluginSpec
return {
  [[hrsh7th/nvim-cmp]],
  name = [[cmp]],
  event = [[InsertEnter]],

  opts = function()
    local cmp = require [[cmp]]
    return {
      sources = cmp.config.sources({
        { name = [[buffer]] },
        { name = [[luasnip]] },
        {
          name = [[nvim_lsp]],
          entry_filter = function(entry)
            return cmp.lsp.CompletionItemKind.Snippet ~= entry:get_kind()
          end
        },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] }
      }),

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          {
            name = [[fuzzy_path]],
            option = {
              fd_timeout_msec = 1500,
              option = fd_opts
            }
          }
        }, {
          { name = [[cmdline]] }
        })
      }),

      snippet = {
        expand = function(args)
          require [[luasnip]].lsp_expand(args.body)
        end
      },

      window = {
        -- https://github.com/clangd/clangd/issues/529
        documentation = false
      },

      -- performance = {
      --   max_view_entries = vim.o.pumheight
      -- },

      mapping = {
        ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { 'i' }),
        ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { 'i' }),
        ['<Tab>'] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Insert,
          select = true
        })
      },

      formatting = {
        fields = { [[abbr]], [[menu]], [[kind]] },
        format = function(entry, cmp_item)
          pcall(function()
            local compl_item = entry:get_completion_item()
            if entry.source.source.client.name == [[clangd]] and compl_item.detail then
              cmp_item.menu = compl_item.detail
            end
          end)

          cmp_item.dup  = 0
          cmp_item.abbr = string.gsub(cmp_item.abbr, [[^%s+]], [[]])
          cmp_item.kind = cmp_item.kind .. [[ ]]

          ---@param field string
          ---@param min_width integer
          ---@param max_width integer
          ---@return nil
          local function clamp(field, min_width, max_width)
            if not cmp_item[field] or not type(cmp_item) == [[string]] then return end
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
