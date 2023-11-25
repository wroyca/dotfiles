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
      desc = [[Mini.Files]]
    }
  },

  config = function()
    local b = true

    local f_s = function(fs_entry) return true end
    local f_h = function(fs_entry) return fs_entry.name ~= [[.DS_Store]] and fs_entry.name ~= [[.git]] and fs_entry.name ~= [[.direnv]] end
    local s_s = function(fs_entry) return require [[mini.files]].default_sort(fs_entry) end
    local s_h = function(fs_entry)
      local p = table.concat(vim.tbl_map(function(t) return t.path end, fs_entry), '\n')
      local i = nil
      local o = {}

      i = vim.fn.jobstart({ [[git]], [[check-ignore]], [[--stdin]] }, {
        stdout_buffered = true, on_stdout = function(_, out) o = out end
      })

      if i < 1 then return fs_entry end

      vim.fn.chansend(i, p)
      vim.fn.chanclose(i, [[stdin]])
      vim.fn.jobwait({ i })

      return require [[mini.files]].default_sort(vim.tbl_filter(function(t)
        return not vim.tbl_contains(o, t.path)
      end, fs_entry))
    end

    local toggle_gitignore = function()
      b = not b
      local f = b and f_h or f_s
      local s = b and s_h or s_s
      require [[mini.files]].refresh({
        content = {
          sort = s,
          filter = f
        }
      })
    end

    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[MiniFilesBufferCreate]],
      callback = function(args)
        local buf_id = args.data.buf_id
        vim.keymap.set([[n]], [[g.]], toggle_gitignore, { buffer = buf_id })
      end
    })

    require [[mini.files]].setup ({
        content = {
          filter = f_h,
          sort = s_h
        }
    })
  end
}
