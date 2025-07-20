---@module "lazydev"
---@module "blink-cmp"

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

  -- Blink completion source for require statements and module annotations.
  {
    "saghen/blink.cmp", optional = true,

    ---@type blink.cmp.Config
    opts = {
      sources = {
        default = { "lazydev", "lsp", "path", "snippets", "buffer" },
        providers = {
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            score_offset = 100,
          },
        },
      },
    },
    opts_extend = { "sources.default", "sources.providers" },
  },
}

return Spec
