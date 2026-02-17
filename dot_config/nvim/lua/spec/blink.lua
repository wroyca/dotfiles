---@module "blink.cmp"

---@type LazyPluginSpec
local Spec = {
  "saghen/blink.cmp", version = "1.*", event = "VeryLazy",

  ---@type blink.cmp.Config
  opts = {
    keymap = {
      preset = "super-tab",
    },
    
    completion = {
      menu = {
        draw = {
          columns = {
            { "label", "label_description" },
          },
        },
      },
    },
    
    cmdline = {
      keymap = { 
        preset = "inherit" 
      },
      
      completion = { 
        menu = { 
          auto_show = true 
        } 
      },
    },

    sources = {
      default = { "lsp", "path", "snippets", "buffer" },

      providers = {
        lsp = {
          transform_items = require("spec.blink.providers.lsp").transform_items,
        },
      },
    },
  },
}

return Spec
