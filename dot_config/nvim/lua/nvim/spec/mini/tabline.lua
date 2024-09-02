---@module "mini.tabline"

---@type LazyPluginSpec
local Spec = {
  "mini.tabline", dev = true, event = "VimEnter", enabled = false,

  opts = {
    tabpage_section = "right",
  }
}

return Spec
