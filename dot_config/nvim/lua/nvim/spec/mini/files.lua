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

  opts = {
    content = {
      sort = function(fs_entries)
        local uncached_dirs = {}

        -- Collect directories that need gitignore caching
        for _, fs_entry in ipairs(fs_entries) do
          local dir = vim.fn.fnamemodify(fs_entry.path, ":h")
          if not MiniFiles.gitignore_is_cached(dir) then
            if not vim.tbl_contains(uncached_dirs, dir) then
              table.insert(uncached_dirs, dir)
            end
          end
        end

        -- Process uncached directories for gitignore
        for _, dir in ipairs(uncached_dirs) do
          local job_id = vim.fn.jobstart({ "git", "check-ignore", "--stdin" }, {
            stdout_buffered = true,
            on_stdout = function(_, files)
              MiniFiles.gitignore_cache(dir, files)
            end,
            on_exit = function()
              -- Trigger a forced refresh to update file visibility
              MiniFiles.force_refresh()
            end
          })

          -- Send directory files to background job
          local files_to_check = {}
          for _, fs_entry in ipairs(fs_entries) do
            if vim.fn.fnamemodify(fs_entry.path, ":h") == dir then
              table.insert(files_to_check, fs_entry.path)
            end
          end

          -- Send file paths and close stdin
          vim.fn.chansend(job_id, table.concat(files_to_check, '\n'))
          vim.fn.chanclose(job_id, "stdin")
        end

        -- Return sorted entries, filtering out ignored files
        return MiniFiles.default_sort(vim.tbl_filter(function(fs_entry)
          local dir = vim.fn.fnamemodify(fs_entry.path, ":h")
          return not MiniFiles.gitignore_is_ignored(dir, fs_entry.path)
        end, fs_entries))
      end,

      filter = function(fs_entry)
        -- Exclude hidden files by default
        return not vim.startswith(fs_entry.name, ".")
      end
    },

    windows = { max_number = 1 },
    options = { permanent_delete = false }
  },

  -- opts shouldn't call setup, as mini modules self-export through _G.
  config = function(_, opts)
    require("mini.files").setup(opts)

    -- Initialize gitignore tracking
    MiniFiles.gitignore = {}
    MiniFiles.gitignore_cache = function(dir, files)
      MiniFiles.gitignore[dir] = MiniFiles.gitignore[dir] or {}
      for _, file in ipairs(files) do
        MiniFiles.gitignore[dir][file] = true
      end
    end

    MiniFiles.gitignore_is_cached = function(dir)
      return MiniFiles.gitignore[dir] ~= nil
    end

    MiniFiles.gitignore_is_ignored = function(dir, file)
      return MiniFiles.gitignore[dir] and MiniFiles.gitignore[dir][file] or false
    end

    -- Toggle between custom and default sorting/filtering
    local function toggle_filtering()
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

    -- Helper function to force refresh the file list after gitignore updates
    function MiniFiles.force_refresh()
      -- Dummy refresh to forcefully update file visibility.
      local function refresh_with_dummy_filter(char)
        MiniFiles.refresh {
          content = {
            filter = function(fs_entry)
              return not vim.startswith(fs_entry.name, char)
            end
          }
        }
      end
      -- MiniFiles.refresh only force updates if there is an actual change.
      refresh_with_dummy_filter(";")
      refresh_with_dummy_filter(".")
    end

    -- Autocommand to set up keybindings for toggling
    vim.api.nvim_create_autocmd("User", {
      pattern = "MiniFilesBufferCreate",
      callback = function(args)
        vim.keymap.set("n", ".", toggle_filtering, { buffer = args.data.buf_id })
      end
    })
  end
}

return Spec
