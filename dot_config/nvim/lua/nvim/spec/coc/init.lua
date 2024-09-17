---@module "coc"

-- TODO: Refactor this code completely at some point. It was hastily written
-- out of annoyance and needs a proper review.
function Gd()
  local function s()
    local l, c = vim.fn.getline("."), vim.fn.col(".")
    local sc = c
    local sd = nil
    while sc > 0 do
      local ch = l:sub(sc, sc)
      if ch == '"' or ch == "'" or ch == "[" then
        sd = ch
        break
      end
      sc = sc - 1
    end
    if not sd then return nil end
    local ec = c
    local ed = nil
    while ec <= #l do
      local ch = l:sub(ec, ec)
      if (sd == '"' and ch == '"') or
         (sd == "'" and ch == "'") or
         (sd == "[" and ch == "]") then
        ed = ch
        break
      end
      ec = ec + 1
    end
    if not ed then return nil end
    return l:sub(sc, ec):gsub("^['\"%[]+", ""):gsub("['\"%]]+$", "")
  end
  local function g(pn)
    local sep = package.config:sub(1, 1)
    local ls = pn:match("([^/]+)$")
    if not ls then return end
    local bp = vim.fn.expand("~/.local/share/nvim/lazy")
    local fp = bp .. sep .. ls
    if vim.fn.isdirectory(fp) == 1 then
      local lfs = vim.fn.glob(fp .. sep .. "**" .. sep .. "*.lua", true, true)
      local qf = {}
      for _, f in ipairs(lfs) do
        table.insert(qf, { filename = f, lnum = 1, text = f })
      end
      vim.fn.setqflist(qf)
      vim.cmd.copen()
    end
  end
  if vim.bo.filetype == "lua" then
    local str = s()
    if str then
      local pn = str
      return g(pn)
    end
  end
  vim.fn.CocActionAsync("jumpDefinition")
end

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
    { "gd", Gd, desc = "Go to definition" },
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
