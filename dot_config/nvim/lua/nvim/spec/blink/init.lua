---@module "blink.cmp"

---@type LazyPluginSpec
local Spec = {
  "saghen/blink.cmp", event = "VeryLazy", build = "cargo build --release",

  ---@type blink.cmp.Config
  opts = {
    windows = {
      autocomplete = {
        max_height = vim.o.pumheight,
        max_width = 50,
        scrolloff = 5,
        draw = "minimal",
      },
      ghost_text = {
        enabled = false,
      },
    },

    trigger = {
      signature_help = {
        enabled = true,
      },
    },

    accept = {
      auto_brackets = {
        enabled = true,
      },
    },
  },
}


return Spec
