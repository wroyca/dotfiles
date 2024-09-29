---@module "hlslens"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-hlslens",

  keys = function(_, keys)
    for _, map in ipairs({ "n", "N" }) do
      table.insert(keys, { map, ([[<cmd>execute('normal! ' . v:count1 . '%s')<cr><cmd>lua require('hlslens').start()<cr>]]):format(map), silent = true })
    end
    for _, map in ipairs({ "/", "*", "#", "g*", "g#" }) do
      table.insert(keys, { map, ([[%s<cmd>lua require('hlslens').start()<cr>]]):format(map) })
    end
    return keys
  end,

  init = function()
    vim.opt.shortmess:append("S")
  end,

  opts = {
    calm_down = true,
  },
}

return Spec
