---@module "mini.surround"

---@type LazyPluginSpec
local Spec = {
  "mini.surround", dev = true, lazy = false,

  keys = function(self, keys)
    local opts = require("lazy.core.plugin").values(self, "opts", false)

    local mappings = {
      { opts.mappings.add },
      { opts.mappings.delete },
      { opts.mappings.replace },
      { opts.mappings.find },
      { opts.mappings.find_left },
      { opts.mappings.highlight },
      { opts.mappings.update_n_lines },
    }

    mappings = vim.tbl_filter(function(m) return m[1] and #m[1] > 0 end, mappings)
    return vim.list_extend(mappings, keys)
  end,

  opts = {
    mappings = {
      add = "gsa",
      delete = "gsd",
      replace = "gsr",
      find = "gsf",
      find_left = "gsF",
      highlight = "gsh",
      update_n_lines = "gsn",
    },
    search_method = "next",
    silent = true,
  },

  specs = {
    "folke/which-key.nvim",
    opts = { spec = { { { "gs", group = "Surround" } } } },
  },
}

return Spec
