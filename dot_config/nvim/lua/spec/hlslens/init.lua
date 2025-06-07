---@module "nvim-hlslens"

---@type LazyPluginSpec
local Spec = {
 "kevinhwang91/nvim-hlslens", event = "VeryLazy",

  init = function()
    vim.opt.shortmess:append ("S")
  end,

  keys = {
    { "n", [[<Cmd>execute("normal! " . v:count1 . "n")<CR>]], desc = "Repeat last search" },
    { "N", [[<Cmd>execute("normal! " . v:count1 . "N")<CR>]], desc = "Repeat last search in the opposite direction" },
    { "*", [[*]], desc = "Search forward for the cursor word" },
    { "#", [[#]], desc = "Search backward for the cursor word" },
    { "g*", [[g*]], desc = "Search forward for the cursor text" },
    { "g#", [[g#]], desc = "Search backward for the cursor text" },
  },

  opts = {
    calm_down = true,
  }
}

return Spec
