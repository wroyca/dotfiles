local M = {}

---@type LazyPluginSpec
return {
  [[mini.files]], name = [[mini-files]], main = [[mini.files]], dev = true,
  keys = {
    {
      [[<leader>f]],
      function()
        local files = require [[mini.files]]
        local file = vim.api.nvim_buf_get_name(0)
        local file_readable = vim.fn.filereadable(file) ~= 0

        files.open()
        -- files.reveal_cwd()
        files.refresh { skip_update_cursor = true,  content = { sort = M.sort, filter = M.filter } }
      end,
      desc = [[Files]]
    }
  },

  opts = {
    windows = { max_number = 1 },
    options = { permanent_delete = false, use_as_default_explorer = true }
  },

  config = function(_, opts)
    local show_dotfiles = true
    local show_gitignore = true

    local sort_hide = function(fs_entries)
      local fs_entries_paths = table.concat(
      vim
      .iter(fs_entries)
      :map(function(fs)
        return fs.path
      end)
      :totable(),
      "\n"
      )

      local job = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
        stdout_buffered = true,
        on_stdout = function(_, out)
          M.stdout = out
        end,
      })

      vim.fn.chansend(job, fs_entries_paths)
      vim.fn.chanclose(job, [[stdin]])
      vim.fn.jobwait({ job })

      return require([[mini.files]]).default_sort(vim
      .iter(fs_entries)
      :filter(function(fs)
        return not vim.tbl_contains(M.stdout, fs.path)
      end)
      :totable())
    end

    local filter_show = function(fs_entry) return true end
    local filter_hide = function(fs_entry)
      return not vim.startswith(fs_entry.name, '.')
    end

    local toggle_gitignore = function()
      show_gitignore = not show_gitignore
      local new_sort = show_gitignore and require([[mini.files]]).default_sort or sort_hide
      MiniFiles.refresh({ content = { sort = new_sort } })
      M.sort = new_sort
    end

    local toggle_dotfiles = function()
      show_dotfiles = not show_dotfiles
      local new_filter = show_dotfiles and filter_show or filter_hide
      MiniFiles.refresh({ content = { filter = new_filter } })
      M.filter = new_filter
      toggle_gitignore()
    end

    vim.api.nvim_create_autocmd('User', {
      pattern = 'MiniFilesBufferCreate',
      callback = function(args)
        local buf_id = args.data.buf_id
        vim.keymap.set('n', '.', toggle_dotfiles, { buffer = buf_id })
      end,
    })

    require [[mini.files]].setup(opts)
  end
}
