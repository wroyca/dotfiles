---@module "mini.colors"

---@type LazyPluginSpec
local Spec = {
  "mini.colors", dev = true, lazy = false,

  config = function()
    vim.cmd.colorscheme("randomhue")
  end
}

return Spec
