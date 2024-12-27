---@type vim.lsp.config
local config = {  
  cmd = {
    "clangd",

    -- Some options are set to their defaults instead of being omitted.
    -- This is to avoid unexpected changes from upstream.
    --
    "--all-scopes-completion=true",
    "--background-index=true",
    "--background-index-priority=normal",
    "--clang-tidy=true",
    "--completion-parse=always",
    "--ranking-model=decision_forest",
    "--completion-style=bundled",
    "--fallback-style=GNU",
    "--function-arg-placeholders=0",
    "--header-insertion=never",
    "--pch-storage=memory",
    "--parse-forwarding-functions",
  },
  
  capabilities = {
    textDocument = {
      inactiveRegionsCapabilities = {
        inactiveRegions = true,
      },
    },
  },
  
  filetypes = { 
    "c", 
    "cpp" 
  },
  
  root_markers = { 
    ".clangd", 
    "compile_commands.json" 
  },
}

return config
