return {
  {
   "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true }
    },
    config = function(_, opts)
      local wk = require("which-key")
      local keymaps = {
        mode = { "n", "v" },
        ["g"] = { name = "+goto" },
        ["gz"] = { name = "+surround" },
        ["]"] = { name = "+next" },
        ["["] = { name = "+prev" },
        ["<leader>f"]= { name = "+file/find" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>u"] = { name = "+ui" },
        ["<leader>w"] = { name = "+windows" },
        ["<leader>x"] = { name = "+diagnostics/quickfix" }
      }
      wk.setup(opts)
      wk.register(keymaps)
    end
  }
}
