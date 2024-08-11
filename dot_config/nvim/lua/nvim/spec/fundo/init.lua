---@module "fundo"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", dependencies = "kevinhwang91/promise-async", event = "VeryLazy",

  init = function ()
    vim.o.undofile = true
  end,

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },

  config = function(_, opts)
    --- Returns true if the given file is older than a week.
    ---@param file string Path to the file.
    ---@return boolean
    local function is_file_older_than_a_week(file)
      local attr = vim.uv.fs_stat(file)
      if attr then
        local current_time = os.time()
        local file_age = current_time - attr.mtime.sec
        return file_age > 7 * 24 * 60 * 60
      end
      return false
    end

    --- Deletes all files and subdirectories in the specified directory.
    ---@param dir string Path to the directory.
    local function wipe_directory(dir)
      for _, file in ipairs(vim.fn.readdir(dir)) do
        local file_path = dir .. "/" .. file
        if vim.fn.isdirectory(file_path) == 1 then
          wipe_directory(file_path)
        else
          vim.fn.delete(file_path)
        end
      end
    end

    --- Checks the fundo archive directory for files older than a week
    --- and wipes the directory if such a file is found.
    ---@param archive_dir string Path to the fundo archive directory.
    local function check_and_wipe(archive_dir)
      for _, file in ipairs(vim.fn.readdir(archive_dir)) do
        local file_path = archive_dir .. "/" .. file
        if is_file_older_than_a_week(file_path) then
          wipe_directory(archive_dir)
          return
        end
      end
    end

    check_and_wipe("/home/wroy/.cache/nvim/fundo")

    require ("fundo").setup(opts)
  end
}

return Spec
