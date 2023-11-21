---@type LazyPluginSpec
return {
  [[folke/noice.nvim]],
  name = [[noice]],
  event = [[LspAttach]],

  opts = {
    cmdline = {
      enabled = false
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
        ["cmp.entry.get_documentation"] = false,
        ["vim.lsp.util.stylize_markdown"] = false,
        ["vim.lsp.util.convert_input_to_markdown_lines"] = false
      }
    }
  }
}
