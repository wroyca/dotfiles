---@type LazyPluginSpec
return {
  [[echasnovski/mini.files]],
  keys = {
    {
      [[<leader>e]],
      function()
        local minifiles = require [[mini.files]]
        if vim.bo.ft == [[minifiles]] then
          minifiles.close()
        else
          local file = vim.api.nvim_buf_get_name(0)
          local file_exists = vim.fn.filereadable(file) ~= 0
          minifiles.open(file_exists and file or nil)
          minifiles.reveal_cwd()
        end
      end,
      desc = [[mini.files]]
    }
  },

  init = function()
    vim.api.nvim_create_autocmd([[user]], {
      pattern = [[MiniFilesWindowOpen]],
      callback = function(args)
        local win_id = args.data.win_id
        vim.api.nvim_win_set_config(win_id, { border = [[double]] })
      end
    })

    vim.api.nvim_create_autocmd("BufEnter", {
      callback = vim.schedule_wrap(function()
        local ft = vim.bo.filetype
        if ft == "minifiles" or ft == "minifiles-help" then return end
        require [[mini.files]].close()
        pcall(vim.api.nvim_set_current_win, vim.api.nvim_get_current_win())
      end)
    })
  end,

  opts = {
    options = {
      permanent_delete = false,
      use_as_default_explorer = true,
    },

    windows = {
      max_number = 1
    },

    content = {
      filter = function(entry)
        return entry.name ~= [[.DS_Store]]
            and entry.name ~= [[.git]]
            and entry.name ~= [[.direnv]]
      end,

      sort = function(entries)
        local all_paths = table.concat(vim.tbl_map(function(entry)
            return entry.path
          end, entries),
          '\n'
        )

        local output_lines = {}
        local job_id = vim.fn.jobstart({
          [[git]],
          [[check-ignore]],
          [[--stdin]]
        }, {
          stdout_buffered = true,
          on_stdout = function(_, data)
            output_lines = data
          end
        })

        if job_id < 1 then
          return entries
        end

        vim.fn.chansend(job_id, all_paths)
        vim.fn.chanclose(job_id, [[stdin]])
        vim.fn.jobwait({ job_id })

        return require [[mini.files]].default_sort(vim.tbl_filter(
          function(entry)
            return not vim.tbl_contains(output_lines, entry.path)
          end,
          entries))
      end
    }
  }
}
