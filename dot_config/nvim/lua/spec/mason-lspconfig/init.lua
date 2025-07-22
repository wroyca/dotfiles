---@module "mason-lspconfig"

-- Neovim 0.11 finally gives us the tools to start language servers directly via
-- `vim.lsp.start()` and related APIs, which means we can now configure servers
-- ourselves without going through `lspconfig` and its ecosystem of helpful-but-
-- occasionally-overbearing defaults.
--
-- Technically, `mason-lspconfig` still insists on having `lspconfig` around.
-- Practically, it only needs it if you use the automatic server setup path. If
-- you're not calling `setup_handlers()` or relying on the plugin to read your
-- mind and configure your editors for you, then `lspconfig` doesn't have to be
-- present. This isn't clearly documented, but works fine once you stop trusting
-- the documentation.
--
-- So we skip it. Instead, we define per-server configuration modules under
-- `lsp/`, and let `mason-lspconfig` find and load them automatically. It turns
-- out this is enough: the plugin installs the server binaries, sees that
-- `lsp/<name>.lua` exists, and steps politely out of the way. For once, things
-- are as simple as they appear.
--
-- The main motivation here is control. `lspconfig` provides convenience, along
-- with a handful of assumptions about how your project is structured, how root
-- directories should be detected, which capabilities the client should support,
-- and which extra features you didn't ask for might be enabled "for your
-- convenience."
--
-- Rather than selectively override half of a prewritten config to claw back
-- predictable behavior, we start from nothing and write what we mean. Said
-- otherwise, we turn `mason-lspconfig` into a declarative server installer.

---@type LazyPluginSpec
local Spec = {
  "mason-org/mason-lspconfig.nvim", dependencies = "mason.nvim", event = "VeryLazy",

  opts = {
    ensure_installed = {
      "lua_ls",
      "clangd",
    },
  },
}

return Spec
