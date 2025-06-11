---@module "fundo"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", dependencies = "kevinhwang91/promise-async", event = "VeryLazy",

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },

  config = function (_, opts)
    require ("fundo").setup (opts)

    -- Fundo stores undo history as individual archive files in the cache
    -- directory. Over time, these can accumulate and consume disk space,
    -- especially if many files are edited infrequently. To prevent this, we
    -- proactively delete stale archives that haven't been modified in over 7
    -- days.
    --
    -- Note that whe whole machinery only involves listing a single directory
    -- and running a cheap `stat` syscall on each file. No file contents are
    -- read, and no recursive traversal is needed. In practice, even with
    -- hundreds of files, the entire cleanup takes less than a millisecond.

    local uv = vim.uv
    local archive_dir = vim.fs.joinpath (vim.fn.stdpath ("cache"), "fundo")

    for _, name in ipairs (vim.fn.readdir (archive_dir) or {}) do
      local path = vim.fs.joinpath (archive_dir, name)
      local stat = uv.fs_stat (path)

      -- Files older than 7 days are considered stale and deleted.
      if stat and stat.mtime and (os.time () - stat.mtime.sec > 7 * 24 * 60 * 60) then
        vim.fn.delete (path)
      end
    end
  end,
}

return Spec
