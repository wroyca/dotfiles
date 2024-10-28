-- Namespace identifier for inactive regions
local inactive_ns = vim.api.nvim_create_namespace('inactive_regions')

---@type LazyPluginSpec
local Spec = {
  "neovim/nvim-lspconfig", lazy = false, enabled = false,

  dependencies = {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
  },

  config = function()
    require("mason-lspconfig").setup_handlers {
      -- The first entry (without a key) will be the default handler
      -- and will be called for each installed server that doesn't have
      -- a dedicated handler.
      function (server_name) require("lspconfig")[server_name].setup {} end,
    }

    local lspconfig = require ("lspconfig")

    lspconfig.clangd.setup {
      -- clang-tools-extra/clangd/tool/ClangdMain.cpp
      cmd = {
        "clangd",

        -- Some options are set to their defaults instead of being omitted.
        -- This is to avoid unexpected changes from upstream.
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

      -- Clangd supports some features that are not in the official Language
      -- Server Protocol specification. https://clangd.llvm.org/extensions
      init_options = {
        clangdFileStatus = true,
      },

      handlers = {
        --- Highlights inactive regions.
        ---
        --- This processes messages containing information about inactive
        --- regions in a document, clears previous highlighting in the
        --- specified buffer, and applies new highlights based on the provided
        --- regions.
        ---
        --- @param message table A message containing the document URI and regions.
        --- - `message.textDocument.uri` (string): URI of the document.
        --- - `message.regions` (table): List of regions to highlight, where each region has `start` and `end` line numbers.
        ['textDocument/inactiveRegions'] = function (_, message, _, _)
          local uri = message.textDocument.uri
          local fname = vim.uri_to_fname(uri)
          local ranges = message.regions

          -- If there are no regions or the buffer doesn't exist, exit early.
          if #ranges == 0 and vim.fn.bufexists(fname) == 0 then return end

          -- Retrieve buffer id; failed that, exit.
          local bufnr = vim.fn.bufadd(fname)
          if not bufnr then return end

          -- Clear previous highlights for this namespace.
          vim.api.nvim_buf_clear_namespace(bufnr, inactive_ns, 0, -1)


          -- Define the new highlight group for inactive regions
          vim.api.nvim_set_hl(0, "InactiveRegion", { fg = "grey", bg = "None", })

          -- Apply new highlights for each region.
          for _, range in ipairs(ranges) do
            local lnum = range.start.line
            local end_lnum = range["end"].line

            vim.api.nvim_buf_set_extmark(bufnr, inactive_ns, lnum, 0, {
              line_hl_group = "InactiveRegion",
              hl_eol = true,
              end_row = end_lnum + 1, -- Highlight includes the last line.
              priority = vim.highlight.priorities.treesitter - 1,
            })
          end
        end
      },

      capabilities = {
        textDocument = {
          -- Manage "textDocument/inactiveRegions" manually, since upstream
          -- implementation is notably slow (they might be using some form of
          -- internal debounce).
          inactiveRegionsCapabilities = {
            inactiveRegions = true,
          },
        },
      },
    }
  end
}

return Spec
