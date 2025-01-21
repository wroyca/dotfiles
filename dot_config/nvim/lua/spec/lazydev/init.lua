---@module "lazydev"

---@type LazyPluginSpec[]
local Spec = {
  {
    "folke/lazydev.nvim", ft = "lua",

    ---@type lazydev.Config
    opts = {
      library = {
        { path = "luvit-meta/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta" },
}

return Spec
