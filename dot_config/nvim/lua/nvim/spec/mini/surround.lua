---@module "mini.surround"

---@type LazyPluginSpec
local Spec = {
  "mini.surround", dev = true,

  keys = {
    { "gsa", mode = { "n", "x" } },
    { "gsd", mode = { "n", "x" } },
    { "gsf", mode = { "n", "x" } },
    { "gsF", mode = { "n", "x" } },
    { "gsh", mode = { "n", "x" } },
    { "gsr", mode = { "n", "x" } },
    { "gsn", demo = { "n", "x" } },
  },

  opts = {
    mappings = {
      add = "gsa",
      delete = "gsd",
      find = "gsf",
      find_left = "gsF",
      highlight = "gsh",
      replace = "gsr",
      update_n_lines = "gsn",
    },
    search_method = "cover_or_next",
    silent = true,
  },
}

return Spec
