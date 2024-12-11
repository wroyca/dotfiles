---@module "mini.nvim"

---@type LazyPluginSpec
local Spec = {
  "echasnovski/mini.nvim", import = "spec.mini",
}

return package.loaded["mini"] and {} or Spec
