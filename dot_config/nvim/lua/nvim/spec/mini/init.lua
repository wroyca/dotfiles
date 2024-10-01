---@module "mini.nvim"

---@type LazyPluginSpec
local Spec = {
  "echasnovski/mini.nvim", import = "nvim.spec.mini"
}

return not package.loaded["mini"] and Spec or {}
