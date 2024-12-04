---@module "coc"

---@type LazyPluginSpec[]
local Spec = {
  {
    "neoclide/coc.nvim", branch = "release", event = "VeryLazy",

    keys = {
      { "<Tab>", [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]], mode = "i", expr = true, replace_keycodes = false, },

      {
        "K",
        function()
          local cw = vim.fn.expand("<cword>")
          if vim.fn.index({ "vim", "help" }, vim.bo.filetype) >= 0 then
            vim.api.nvim_command("h" .. " " .. cw)
          elseif vim.api.nvim_eval("coc#rpc#ready()") and vim.fn.CocAction("hasProvider", "hover") then
            vim.fn.CocActionAsync("doHover")
          else
            vim.fn.feedkeys("K", "in")
          end
        end,
      },

      { "gD", "<Plug>(coc-declaration)", desc = "Go to declaration" },
      { "gd", "<Plug>(coc-definition)", desc = "Go to definition" },
      { "gi", "<Plug>(coc-implementation)", desc = "Go to implementation" },
      { "gr", "<Plug>(coc-references)", desc = "Go to references" },
      { "gt", "<Plug>(coc-type-definition)", desc = "Go to type definition" },

      { "<leader>.", "<Plug>(coc-codeaction-cursor)", desc = "Code action", mode = { "n", "x" }, nowait = true },
      { "<leader>;", "<Plug>(coc-format-selected)", desc = "Format selection", mode = { "n", "x" }, nowait = true },
    },

    opts = {
      colors = {
        enable = true,
      },

      diagnostic = {
        enableHighlightLineNumber = false,
        enableSign = false,
        floatConfig = {
          focusable = false,
        },
        refreshOnInsertMode = true,
      },

      dialog = {
        floatBorderHighlight = "FloatBorder",
      },

      hover = {
        floatConfig = {
          focusable = true,
        },
      },

      inlayHint = {
        enable = false,
      },

      list = {
        alignColumns = true,
        floatPreview = {
          focusable = false,
        },
        statusLineSegments = false,
      },

      notification = {
        disabledProgressSources = {
          "*",
        },
      },

      semanticTokens = {
        enable = true,
      },

      signature = {
        floatConfig = {
          focusable = false,
        },
      },

      suggest = {
        asciiCharactersOnly = true,
        floatConfig = {
          focusable = false,
        },
        removeDuplicateItems = true,
      },

      coc = {
        preferences = {
          currentFunctionSymbolAutoUpdate = true,
          enableLinkedEditing = true,
          formatOnType = true,
        },
      },

      snippets = {
        loadFromExtensions = false,
        priority = 100,
        textmateSnippetsRoots = {
          vim.fs.joinpath(vim.fn.stdpath("config") --[[@as string]], "snippets"),
        },
      },

      clangd = {
        arguments = {
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
        disableSnippetCompletion = true
      },

      Lua = {
        runtime = {
          version = "LuaJIT",
        },
        workspace = {
          checkThirdParty = false,
          library = {
            "/usr/share/nvim/runtime",
            vim.fs.joinpath(vim.fn.stdpath("data") --[[@as string]], "lazy"),
          },
        },
      },
    },

    config = function(opts)
      vim.g.coc_global_extensions = {
        "coc-marketplace",
        "coc-clangd",
        "coc-json",
        "coc-lua",
        "coc-sh",
        "coc-xml",
        "coc-snippets",
        "@statiolake/coc-stylua",
      }

      for k, v in pairs(opts.opts --[[@as table]]) do
        vim.fn["coc#config"](k, v)
      end
    end,
  },

  -- https://github.com/neovim/neovim/issues/26268
  { "Bilal2453/luvit-meta" },
}

return Spec
