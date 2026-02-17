---@diagnostic disable: missing-fields
---@type vim.lsp.config
local config = {
  settings = {
    Lua = {
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
          { path = "${3rd}/luv/library", ords = { "vim%.uv" } },
        },
      },
    },
  },
}

return config
