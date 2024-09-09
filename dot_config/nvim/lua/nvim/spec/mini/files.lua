---@module "mini.files"

---@type LazyPluginSpec
local Spec = {
  "mini.files", dev = true, dependencies = {{ "mini.icons", dev = true }},

  keys = {{
    "<leader>f",
    function()
      local file = vim.api.nvim_buf_get_name(0)
      local file_exists = vim.fn.filereadable(file) ~= 0

      MiniFiles.open(file_exists and file or nil)
      MiniFiles.reveal_cwd()
      MiniFiles.refresh({
        content = {
          sort = MiniFiles.sort,
          filter = MiniFiles.filter,
        },
      })
    end,
    desc = "Files"
  }},

  -- TODO:
  --
  -- Cache invalidation needs to be addressed, potentially right after
  -- synchronization. To achieve this we first need the machinery to notify
  -- when processing is complete (see the FIXME below).
  --
  opts = {
    content = {
      sort = function(fs_entries)
        local dirs = {}
        for _, fs in ipairs(fs_entries) do
          local dir = vim.fn.fnamemodify(fs.path, ":h")
          if not MiniFiles.gitignore_is_cached(dir) then
            if not vim.tbl_contains(dirs, dir) then
              table.insert(dirs, dir)
            end
          end
        end
        for _, dir in ipairs(dirs) do
          local id = vim.fn.jobstart({ "git", "check-ignore", "--stdin" }, {
            stdout_buffered = true,
            on_stdout = function(_, file)
              MiniFiles.gitignore_cache(dir, file)
            end,
          })
          local entries = {}
          for _, fs in ipairs(fs_entries) do
            if vim.fn.fnamemodify(fs.path, ":h") == dir then
              table.insert(entries, fs.path)
            end
          end
          -- This operation is non-blocking. Smaller repositories will
          -- filter out .gitignored files instantly, while large ones (e.g.,
          -- Chromium) will process in the background.
          --
          -- FIXME: There's nothing to notify when the processing is
          -- complete.
          --
          vim.fn.chansend(id, table.concat(entries, '\n'))
          vim.fn.chanclose(id, "stdin")
        end
        return MiniFiles.default_sort(vim.iter(fs_entries):filter(function(fs)
          local dir = vim.fn.fnamemodify(fs.path, ":h")
          return not MiniFiles.gitignore_is_ignored(dir, fs.path)
        end):totable())
      end,

      filter = function(fs_entry)
        return not vim.startswith(fs_entry.name, ".")
      end
    },

    windows = { max_number = 1 },
    options = { permanent_delete = false }
  },

  config = function(_, opts)
    require ("mini.files").setup(opts)

    MiniFiles.gitignore = {}
    MiniFiles.gitignore_cache = function(dir, files) MiniFiles.gitignore[dir] = MiniFiles.gitignore[dir] or {} for _, file in ipairs(files) do MiniFiles.gitignore[dir][file] = true end end
    MiniFiles.gitignore_is_cached = function(dir) return MiniFiles.gitignore[dir] ~= nil end
    MiniFiles.gitignore_is_ignored = function(dir, file)
      return MiniFiles.gitignore[dir] and MiniFiles.gitignore[dir][file] or false
    end

    local function toggle_state()
      MiniFiles.state = not MiniFiles.state
      MiniFiles.sort = MiniFiles.state and MiniFiles.default_sort or opts.content.sort
      MiniFiles.filter = MiniFiles.state and MiniFiles.default_filter or opts.content.filter
      MiniFiles.refresh {
        content = {
          sort = MiniFiles.sort,
          filter = MiniFiles.filter
        }
      }
    end

    vim.api.nvim_create_autocmd("User", {
      pattern = "MiniFilesBufferCreate",
      callback = function(args)
        vim.keymap.set("n", ".", toggle_state, { buffer = args.data.buf_id })
      end
    })
  end
}

return Spec
