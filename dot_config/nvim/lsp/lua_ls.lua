---@diagnostic disable: missing-fields
---@type vim.lsp.config
local config = {
  cmd = {
    "lua-language-server",
  },

  filetypes = {
    "lua",
  },

  root_markers = {
    ".luarc.json",
    ".luarc.jsonc",
    ".stylua.toml",
    "stylua.toml",
  },
}

return config
