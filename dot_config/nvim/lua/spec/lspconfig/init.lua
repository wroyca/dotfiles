---@module "lspconfig"

---@type LazyPluginSpec
local Spec = {
  "neovim/nvim-lspconfig", event = "User AsyncFileLoad",

  dependencies = {
    "mason.nvim",
    "mason-lspconfig.nvim",
  },

  opts = function ()
    -- Helper function to safely load LSP config files
    local function load_lsp_config (name)
      local config_files = vim.api.nvim_get_runtime_file ("lsp/" .. name .. ".lua", true)
      if #config_files > 0 then
        local ok, config = pcall (dofile, config_files[1])
        if ok then return config end
      end
      return nil
    end

    return {
      clangd = load_lsp_config ("clangd"),
      lua_ls = load_lsp_config ("lua_ls")
    }
  end,

  config = function (_, opts)
    vim.api.nvim_create_autocmd ("LspAttach", {
      callback = function (args)
        local client = vim.lsp.get_client_by_id (args.data.client_id)

        -- Helper function to create keymaps for supported LSP methods
        local function create_keymap_if_supported (method, keymap, callback, description)
          if client and client:supports_method (method) then
            vim.keymap.set ("n", keymap, callback, { buffer = args.buf, desc = description })
          end
        end

        create_keymap_if_supported ("textDocument/declaration",    "gd",         vim.lsp.buf.declaration,     "Go to Declaration")
        create_keymap_if_supported ("textDocument/definition",     "gD",         vim.lsp.buf.definition,      "Go to Definition")
        create_keymap_if_supported ("textDocument/implementation", "gi",         vim.lsp.buf.implementation,  "Go to Implementation")
        create_keymap_if_supported ("textDocument/references",     "gr",         vim.lsp.buf.references,      "Go to References")
        create_keymap_if_supported ("textDocument/typeDefinition", "gt",         vim.lsp.buf.type_definition, "Go to Type Definition")
        create_keymap_if_supported ("textDocument/codeAction",     "<leader>.",  vim.lsp.buf.code_action,     "Code Action")
      end,
    })

    require("mason-lspconfig").setup_handlers {}
    for server_name, server_opts in pairs (opts) do
      require ("lspconfig")[server_name].setup (server_opts)
    end
  end,
}

return Spec
