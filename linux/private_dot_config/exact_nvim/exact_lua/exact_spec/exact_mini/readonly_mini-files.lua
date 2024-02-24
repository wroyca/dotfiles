local content = {}

---@type LazyPluginSpec
return {
  [[mini.files]],
  keys = {
    {
      [[<leader>f]],
      function()
        local MiniFiles = require [[mini.files]]
        if vim.bo.ft == [[minifiles]] then
          MiniFiles.close()
        else
          local file = vim.api.nvim_buf_get_name(0)
          local file_exists = vim.fn.filereadable(file) ~= 0
          MiniFiles.open(file_exists and file or nil)
          MiniFiles.reveal_cwd()
          MiniFiles.refresh { content = { sort = content.sort, filter = content.filter } }
        end
      end,
      desc = [[Files]]
    }
  },

  opts = {
    content = {
      sort = function(fs_entries)
        local path = table.concat(vim.iter(fs_entries):map(function(fs) return fs.path end):totable(), '\n')
        local stdout = {}
        local cmd = vim.fn.jobstart ({ [[git]], [[check-ignore]], [[--stdin]] }, {
          stdout_buffered = true, on_stdout = function(_, out) stdout = out end,
        })

        vim.fn.chansend(cmd, path)
        vim.fn.chanclose(cmd, [[stdin]])
        vim.fn.jobwait{cmd}

        return require [[mini.files]].default_sort(vim.iter(fs_entries):filter(function(fs)
          return not vim.tbl_contains(stdout, fs.path)
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
    local state = true
    local toggle_state = function()
      state = not state
      content.sort = state and opts.content.sort or function(_) return require([[mini.files]]).default_sort(_) end
      content.filter = state and opts.content.filter or function(_) return true end
      require [[mini.files]].refresh { content = { sort = content.sort, filter = content.filter } }
    end
    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[MiniFilesBufferCreate]],
      callback = function(args)
        vim.keymap.set([[n]], [[.]], toggle_state, { buffer = args.data.buf_id })
      end
    })
    require [[mini.files]].setup(opts)
  end
}
