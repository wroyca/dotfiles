---@module "nvim-highlight-colors"

---@type LazyPluginSpec
local Spec = {
  "brenoprata10/nvim-highlight-colors", event = "VeryLazy",
  opts = {
    enable_tailwind = true,
  }
}

return Spec
