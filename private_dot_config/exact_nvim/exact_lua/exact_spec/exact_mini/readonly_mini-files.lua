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
      end
    }
  },

  config = function()
    local map = vim.tbl_map
    local cat = table.concat
    local filter = vim.tbl_filter
    local contains = vim.tbl_contains

    local f_s = function(_) return true end
    local f_h = function(fs_entry)
      return fs_entry.name ~= [[.DS_Store]]
        and fs_entry.name ~= [[.git]]
      and fs_entry.name ~= [[.direnv]]
    end

    local s_s = function(fs_entries) return require([[mini.files]]).default_sort(fs_entries) end
    local s_h = function(fs_entries)
      local p = cat(map(function(e) return e.path end, fs_entries), "\n")
      local i = nil
      local o = {}

      i = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
        stdout_buffered = true, on_stdout = function(_, out) o = out end,
      })

      if i < 1 then return fs_entries end

      vim.fn.chansend(i, p)
      vim.fn.chanclose(i, [[stdin]])
      vim.fn.jobwait { i }

      -- stylua: ignore
      return require([[mini.files]]).default_sort(filter(function(e)
        return not contains(o, e.path)
      end, fs_entries))
    end

    local b = false
    local toggle_gitignore = function()
      b = not b
      local f = b and f_h or f_s
      local s = b and s_h or s_s
      require([[mini.files]]).refresh { content = { sort = s, filter = f } }
    end

    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[MiniFilesBufferCreate]],
      callback = function(args)
        local buf_id = args.data.buf_id
        vim.keymap.set([[n]], [[g.]], toggle_gitignore, { buffer = buf_id })
      end
    })

    vim.api.nvim_create_autocmd([[BufEnter]], {
      callback = vim.schedule_wrap(function()
        local ft = vim.bo.filetype

        if ft == [[minifiles]] or ft == [[minifiles-help]] then
          return
        end

        require [[mini.files]].close()
        pcall(vim.api.nvim_set_current_win, vim.api.nvim_get_current_win())
      end)
    })

    require [[mini.files]].setup {
      content = { filter = f_h, sort = s_h },
      options = { permanent_delete = false, use_as_default_explorer = true },
      windows = { max_number = 1 }
    }
  end
}
