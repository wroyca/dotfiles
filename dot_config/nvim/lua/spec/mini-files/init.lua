local M = {}

---@type LazyPluginSpec
return {
  [[mini.files]], name = [[mini-files]], main = [[mini.files]], dev = true,
    keys = {
    {
      [[<leader>f]],
      function()
        local file = vim.api.nvim_buf_get_name(0)
        local file_exists = vim.fn.filereadable(file) ~= 0

        -- The API expect a valid file system path as the anchor. Now,
        -- `nvim_buf_get_name` doesn't validate the file system path; it
        -- returns the buffer name. To ensure validation, `vim.fn.filereadable`
        -- is used and if the file doesn't exists, the API will use it own
        -- fallback.

        require [[mini.files]].open(file_exists and file or nil)
        require [[mini.files]].reveal_cwd()
        require [[mini.files]].refresh { content = { sort = M.sort, filter = M.filter } }
      end,
      desc = [[Files]]
    }
  },

  opts = {
    content = {
      -- https://github.com/echasnovski/mini.nvim/issues/377#issuecomment-1669965688
      sort = function(fs_entries)
        local fs_entries_paths = table.concat(vim.iter(fs_entries):map(function(fs)
          return fs.path
        end):totable(), '\n')

        local job = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
          stdout_buffered = true,
          on_stdout = function(_, out)
            M.stdout = out
          end
        })

        vim.fn.chansend(job, fs_entries_paths)
        vim.fn.chanclose(job, [[stdin]])
        vim.fn.jobwait({ job })

        return require [[mini.files]].default_sort(vim.iter(fs_entries):filter(function(fs)
          return not vim.tbl_contains(M.stdout, fs.path)
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
    local toggle_state = function()
      M.state = not M.state
      M.sort = M.state and require [[mini.files]].default_sort or opts.content.sort
      M.filter = M.state and require [[mini.files]].default_filter or opts.content.filter
      require [[mini.files]].refresh {
        content = {
          sort = M.sort,
          filter = M.filter
        }
      }
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
