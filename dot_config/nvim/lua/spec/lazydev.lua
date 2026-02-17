---@module "lazydev"

---@type LazyPluginSpec
local Spec = {
  "folke/lazydev.nvim", ft = "lua",

  ---@type lazydev.Config
  opts = {
    library = {
      { path = "${3rd}/luv/library", words = { "vim%.uv" } },
    },
  },
}

return Spec
