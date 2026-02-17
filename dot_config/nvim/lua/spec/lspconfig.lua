---@module "mason"

---@type LazyPluginSpec
local Spec = {
  "neovim/nvim-lspconfig",
  
  keys = {
    {
      "<leader>la",
      vim.lsp.buf.code_action,
      desc = "Actions",
    },
    {
      "<leader>ld",
      vim.diagnostic.open_float,
      desc = "Diagnostic",
    },
    {
      "<leader>li",
      vim.lsp.buf.implementation,
      desc = "Implementation",
    },
    {
      "<leader>lr",
      vim.lsp.buf.rename,
      desc = "Rename",
    },
    {
      "<leader>lR",
      vim.lsp.buf.references,
      desc = "References",
    },
    {
      "<leader>ls",
      vim.lsp.buf.definition,
      desc = "Source definition",
    },
    {
      "<leader>lt",
      vim.lsp.buf.type_definition,
      desc = "Type definition",
    },
  },

  -- The standard `nvim-lspconfig` loading behavior is problematic for our
  -- setup because it aggressively registers its own collection of language
  -- server configurations. These upstream configs reside in the module
  -- namespace and tend to shadow our local user-defined modules (e.g.,
  -- `lsp/clangd.lua`). If we simply let the plugin load normally, the package
  -- loader finds the plugin's version first, effectively ignoring our
  -- customizations.
  --
  -- To handle this, we treat the plugin as a library rather than a service. We
  -- want the code available on disk and in the Lua path, but we want to avoid
  -- the side effects of its default initialization logic.

  init = function ()
    vim.opt.runtimepath:prepend (require ("lazy.core.config").options.root .. "/nvim-lspconfig")
  end,
}

return Spec
