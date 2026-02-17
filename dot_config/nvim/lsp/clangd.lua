---@diagnostic disable: missing-fields
---@type vim.lsp.config
local config = {
  cmd = {
    "clangd",
    "--all-scopes-completion=true",
    "--background-index-priority=normal",
    "--background-index=true",
    "--clang-tidy=true",
    "--completion-parse=always",
    "--completion-style=bundled",
    "--fallback-style=GNU",
    "--function-arg-placeholders=0",
    "--header-insertion=never",
    "--parse-forwarding-functions",
    "--pch-storage=memory",
    "--ranking-model=decision_forest"
  },

  capabilities = {
    textDocument = {
      completion = {
        completionItem = {
          snippetSupport = false,
        },
      },
      inactiveRegionsCapabilities = {
        inactiveRegions = true,
      },
    },
  },

  filetypes = {
    "c",
    "cpp",
  },

  root_dir = function (bufnr, ondir)
    return ondir (require ("lspconfig.util").root_pattern ("compile_commands.json") (vim.api.nvim_buf_get_name (bufnr)))
  end,
}

return config
