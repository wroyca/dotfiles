local s
local f

---@type LazyPluginSpec
return {
  [[mini.files]],
  keys = {
    {
      [[<leader>f]],
      function()
        require [[mini.files]].open(vim.api.nvim_buf_get_name(0))
        require [[mini.files]].reveal_cwd()
        require [[mini.files]].refresh { content = { sort = s, filter = f } }
      end,
      desc = [[Files]]
    }
  },

  opts = {
    content = {
      sort = function(fs_entries)
        local p = table.concat(vim.iter(fs_entries):map(function(fs) return fs.path end):totable(), '\n')
        local o = {}
        local i = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
          stdout_buffered = true, on_stdout = function(_, out) o = out end
        })

        vim.fn.chansend(i, p)
        vim.fn.chanclose(i, [[stdin]])
        vim.fn.jobwait { i }

        return require([[mini.files]]).default_sort(vim.iter(fs_entries):filter(function(fs)
          return not vim.tbl_contains(o, fs.path)
        end):totable())
      end,

      filter = function(fs_entry)
        return not vim.startswith(fs_entry.name, [[.]])
      end
    },

    windows = { max_number = 1 },
    options = { permanent_delete = false, use_as_default_explorer = true }
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
