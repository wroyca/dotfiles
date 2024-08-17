---@module "fundo"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-fundo", dependencies = "kevinhwang91/promise-async", event = "VeryLazy",

  init = function()
    vim.o.undofile = true
  end,

  ---@type FundoConfig
  opts = {
    limit_archives_size = 9999,
  },

  config = function(_, opts)
    require("fundo").setup(opts)

    local archive_dir = "/home/wroy/.cache/nvim/fundo"
    for _, file in ipairs(vim.fn.readdir(archive_dir)) do
      local stat = vim.uv.fs_stat(file)
      if stat and os.time() - stat.mtime.sec > 7 * 24 * 60 * 60 then
        vim.fn.delete(file)
      end
    end
  end,
}

return Spec
