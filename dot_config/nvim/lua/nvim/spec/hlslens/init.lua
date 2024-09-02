---@module "hlslens"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-hlslens", keys = { "*", "#", "n", "N" },

  init = function()
    vim.opt.shortmess:append("S")
  end,

  opts = {
    calm_down = true,
  },
}

return Spec
