---@module "mini.files"

---@type LazyPluginSpec
local Spec = {
  "mini.files", dev = true,
  keys = {
    {
      "<leader>f",
      function()
        local file = vim.api.nvim_buf_get_name(0)
        local file_exists = vim.fn.filereadable(file) ~= 0

        require("mini.files").open(file_exists and file or nil)
        require("mini.files").reveal_cwd()
        require("mini.files").refresh { content = { sort = MiniFilesConfig.sort, filter = MiniFilesConfig.filter } }
      end,
      desc = "Files"
    }
  },

  opts = {
    content = {
      -- https://github.com/echasnovski/mini.nvim/issues/377#issuecomment-1669965688
      sort = function(fs_entries)
        local fs_entries_paths = table.concat(vim.iter(fs_entries):map(function(fs)
          return fs.path
        end):totable(), '\n')

        local job = vim.fn.jobstart({ "git", "check-ignore", "--stdin" }, {
          stdout_buffered = true,
          on_stdout = function(_, out)
            MiniFilesConfig.stdout = out
          end
        })

        vim.fn.chansend(job, fs_entries_paths)
        vim.fn.chanclose(job, "stdin")
        vim.fn.jobwait({ job })

        return require("mini.files").default_sort(vim.iter(fs_entries):filter(function(fs)
          return not vim.tbl_contains(MiniFilesConfig.stdout, fs.path)
        end):totable())
      end,

      filter = function(fs_entry)
        return not vim.startswith(fs_entry.name, ".")
      end
    },

    windows = { max_number = 1 },
    options = { permanent_delete = false, use_as_default_explorer = true }
  },

  config = function(_, opts)
    MiniFilesConfig = MiniFilesConfig or { state = false }

    local function toggle_state()
      MiniFilesConfig.state = not MiniFilesConfig.state
      MiniFilesConfig.sort = MiniFilesConfig.state and require("mini.files").default_sort or opts.content.sort
      MiniFilesConfig.filter = MiniFilesConfig.state and require("mini.files").default_filter or opts.content.filter
      require("mini.files").refresh {
        content = {
          sort = MiniFilesConfig.sort,
          filter = MiniFilesConfig.filter
        }
      }
    end

    vim.api.nvim_create_autocmd("User", {
      pattern = "MiniFilesBufferCreate",
      callback = function(args)
        vim.keymap.set("n", ".", toggle_state, { buffer = args.data.buf_id })
      end
    })

    require("mini.files").setup(opts)
  end
}

return Spec
