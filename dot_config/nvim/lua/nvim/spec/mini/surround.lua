---@module "mini.surround"

---@type LazyPluginSpec
local Spec = {
  "mini.surround", dev = true,

  keys = {
    { "gsa", mode = { "n", "x" } },
    { "gsd", mode = { "n", "x" } },
    { "gsr", mode = { "n", "x" } },
    { "gsf", mode = { "n", "x" } },
    { "gsF", mode = { "n", "x" } },
    { "gsh", mode = { "n", "x" } },
    { "gsn", mode = { "n", "x" } },
  },

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
}

return Spec
