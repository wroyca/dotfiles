---@module "coc"

---@type LazyPluginSpec
local Spec = {
  "neoclide/coc.nvim", branch = "release", event = "VeryLazy",

  -- https://github.com/neovim/neovim/issues/26268
  dependencies = { { "Bilal2453/luvit-meta", lazy = true } },

  keys = {
    { "<Tab>", [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]], mode = "i", expr = true, replace_keycodes = false },

    {
      "K",
      function()
        local cw = vim.fn.expand("<cword>")
        if vim.fn.index({ "vim", "help" }, vim.bo.filetype) >= 0 then
          vim.api.nvim_command("h" .. " " .. cw)
        elseif vim.api.nvim_eval("coc#rpc#ready()") and vim.fn.CocAction("hasProvider", "hover") then
          vim.fn.CocActionAsync "doHover"
        else
          vim.fn.feedkeys("K", "in")
        end
      end
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
      enable = false
    },

    diagnostic = {
      enableHighlightLineNumber = false,
      enableSign = false,
      floatConfig = {
        focusable = false
      },
      refreshOnInsertMode = true,
    },

    dialog = {
      floatBorderHighlight = "FloatBorder"
    },

    hover = {
      floatConfig = {
        focusable = false
      }
    },

    inlayHint = {
      enable = false
    },

    list = {
      alignColumns = true,
      floatPreview = {
        focusable = false
      },
      statusLineSegments = false,
    },

    notification = {
      disabledProgressSources = {
        "*"
      }
    },

    semanticTokens = {
      enable = false
    },

    signature = {
      floatConfig = {
        focusable = false,
      }
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

    clangd = {
      arguments = {
        "--all-scopes-completion=true",
        "--background-index-priority=normal",
        "--background-index=true",
        "--clang-tidy",
        "--completion-parse=always",
        "--completion-style=bundled",
        "--function-arg-placeholders=false",
        "--header-insertion=never",
        "--parse-forwarding-functions",
        "--pch-storage=memory",
        "--ranking-model=decision_forest",
      },
    },

    Lua = {
      runtime = {
        version = "LuaJIT",
      },
      workspace = {
        checkThirdParty = false,
        library = {
          "/usr/share/nvim/runtime",

          vim.fs.joinpath(vim.fn.stdpath("data") --[[@as string]], "lazy")
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
      "@statiolake/coc-stylua",
    }

    for k, v in pairs(opts.opts --[[@as table]]) do
      vim.fn["coc#config"](k, v)
    end
  end
}

return Spec
