local s
local f

---@type LazyPluginSpec
return {
  [[echasnovski/mini.files]],

  keys = {
    {
      [[<leader>e]],
      function()
        require [[mini.files]].open(vim.api.nvim_buf_get_name(0))
        require [[mini.files]].reveal_cwd()
        require [[mini.files]].refresh { content = { sort = s, filter = f } }
      end,
      desc = [[Mini Files]]
    }
  },

  opts = {
    content = {
      sort = function(fs_entries)
        local p = table.concat(vim.iter(fs_entries):map(function(e) return e.path end):totable(), '\n')
        local o = {}
        local i = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
          stdout_buffered = true,
          on_stdout = function(_, out)
            o = out
          end
        })

        vim.fn.chansend(i, p)
        vim.fn.chanclose(i, [[stdin]])
        vim.fn.jobwait { i }
        return require([[mini.files]]).default_sort(vim.iter(fs_entries):filter(function(e)
          return not vim.tbl_contains(o, e.path)
        end):totable())
      end,

      filter = function(fs_entry)
        return not vim.startswith(fs_entry.name, '.')
      end
    },

    options = { permanent_delete = false, use_as_default_explorer = true },
    windows = { max_number = 1 } -- Miller columns suck, sorry.
  },

  config = function(_, opts)
    local b = true
    local toggle_gitignore = function()
      b = not b
      s = b and opts.content.sort   or function(_) return require([[mini.files]]).default_sort(_) end
      f = b and opts.content.filter or function(_) return true end
      require [[mini.files]].refresh { content = { sort = s, filter = f } }
    end

    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[MiniFilesBufferCreate]],
      callback = function(args)
        vim.keymap.set([[n]], [[.]], toggle_gitignore, { buffer = args.data.buf_id })
      end
    })

    require [[mini.files]].setup(opts)
  end
}
