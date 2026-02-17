---@module "fundo"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", event = "VeryLazy",

  ---@type FundoConfig
  opts = {
    -- We set the internal archive limit to an effectively infinite value.
    --
    -- The rationale is that the plugin's native eviction strategy is based on
    -- the *count* of archives (files). This is suboptimal for our workflow:
    -- opening a large number of files in a short span shouldn't wipe out the
    -- undo history of a file we edited yesterday.
    --
    -- Instead, we disable the count-based limit here and implement a
    -- time-based retention policy (removing old history) in the config
    -- function below.
    --
    limit_archives_size = 9999,
  },

  config = function (_, opts)
    require ("fundo").setup (opts)

    -- Fundo maintains undo history by writing each editing session to a
    -- separate archive file within the cache directory. This avoids contention
    -- and allows for robust recovery, but over time these archives may
    -- accumulate, particularly in projects where many files are edited
    -- sporadically.
    --
    -- To control storage growth, we implement a custom cleanup pass here. We
    -- remove archives that have not been modified in over seven days. This
    -- retention policy is currently hardcoded but seems like a reasonable
    -- balance between disk usage and history availability.
    --
    local uv = vim.uv
    local archive_dir = vim.fs.normalize (vim.fs.joinpath (vim.fn.stdpath ("cache"), "fundo"))

    -- We use synchronous directory reading here.
    --
    -- While generally we try to avoid blocking the main thread, the overhead
    -- of listing a single directory (even with a few thousand entries) is
    -- negligible: That is, it involves listing the top-level directory and
    -- applying a metadata query (`stat`) to each file. The contents are never
    -- read.
    --
    local entries = vim.fn.readdir (archive_dir) or {}

    -- Calculate the cutoff once to avoid re-evaluating os.time() in the loop.
    --
    local now = os.time ()
    local retention_period = 7 * 24 * 60 * 60

    for _, name in ipairs (entries) do
      local path = vim.fs.normalize (vim.fs.joinpath (archive_dir, name))
      local stat = uv.fs_stat (path)

      if stat and stat.mtime and (now - stat.mtime.sec > retention_period) then
        vim.fn.delete (path)
      end
    end
  end,
}

return Spec
