---@type LazyPluginSpec
return {
  [[folke/noice.nvim]],
  event = [[VeryLazy]],

  opts = {
    cmdline = {
      enabled = false,
    },

    messages = {
      enabled = false
    },

    popupmenu = {
      enabled = false
    },

    lsp = {
      progress = {
        enabled = true
      },

      message = {
        enabled = true
      },

      override = {
        ["cmp.entry.get_documentation"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true
      }
    }
  }
}
