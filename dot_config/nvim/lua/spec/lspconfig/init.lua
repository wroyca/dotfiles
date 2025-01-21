---@module "lspconfig"

---@type LazyPluginSpec
local Spec = {
  "neovim/nvim-lspconfig", event = "User AsyncFileLoad",

  dependencies = {
    "mason.nvim",
    "mason-lspconfig.nvim",
  },

  -- FIXME: This uses a direct hack to load LSP config files by searching for
  -- and executing them at runtime. It will lead to subtle and unpredictable
  -- issues when used with lazy.nvim.
  opts = {
    clangd = loadfile (unpack (vim.api.nvim_get_runtime_file ("lsp/clangd.lua", true))) (),
    lua_ls = loadfile (unpack (vim.api.nvim_get_runtime_file ("lsp/lua_ls.lua", true))) (),
  },

  config = function (_, opts)
    vim.api.nvim_create_autocmd ("LspAttach", {
      callback = function (args)
        local client = vim.lsp.get_client_by_id (args.data.client_id)

        -- Helper function to create keymaps for supported LSP methods
        local function create_keymap_if_supported (method, keymap, callback, description)
          if client:supports_method (method) then
            vim.keymap.set ("n", keymap, callback, { buffer = args.buf, desc = description })
          end
        end

        create_keymap_if_supported ("textDocument/declaration",    "<leader>ld", vim.lsp.buf.declaration,     "Go to Declaration")
        create_keymap_if_supported ("textDocument/definition",     "<leader>lD", vim.lsp.buf.definition,      "Go to Definition")
        create_keymap_if_supported ("textDocument/implementation", "<leader>li", vim.lsp.buf.implementation,  "Go to Implementation")
        create_keymap_if_supported ("textDocument/references",     "<leader>lr", vim.lsp.buf.references,      "Go to References")
        create_keymap_if_supported ("textDocument/typeDefinition", "<leader>lt", vim.lsp.buf.type_definition, "Go to Type Definition")
      end,
    })

    for server_name, server_opts in pairs (opts) do
      require ("lspconfig")[server_name].setup (server_opts)
    end
  end,
}

return Spec
