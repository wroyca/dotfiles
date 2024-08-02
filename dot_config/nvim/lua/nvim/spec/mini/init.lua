---@type LazyPluginSpec[]
local Spec = {
  { "echasnovski/mini.nvim", lazy = false },

  -- Import (but don't load) all mini modules specifications.
  {
    import = "nvim.spec.mini"
  }
}

return not package.loaded['mini'] and Spec or {}
