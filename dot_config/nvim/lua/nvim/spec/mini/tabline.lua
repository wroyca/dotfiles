---@module "mini.tabline"

---@type LazyPluginSpec
local Spec = {
  "mini.tabline", dev = true, event = "VimEnter",

  opts = {
    tabpage_section = "right",
  }
}

return Spec
