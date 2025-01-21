---@module "blink.cmp"

---@type LazyPluginSpec
local Spec = {
  "saghen/blink.cmp", build = "cargo build --release", event = "VeryLazy",

  ---@type blink.cmp.Config
  opts = {
    keymap = { preset = "super-tab" },
    completion = {
      menu = {
        draw = {
          columns = {
            { "label", "label_description" },
          },
        },
      },
    },
    sources = {
      default = { "lsp", "path", "snippets", "buffer" },
    },
  },
  opts_extend = { "sources.default" },
}

return Spec
