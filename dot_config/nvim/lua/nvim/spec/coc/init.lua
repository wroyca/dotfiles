---@type LazyPluginSpec
local Spec = {
  "neoclide/coc.nvim", build = "npm install", event = "VeryLazy",

  init = function ()
    vim.o.updatetime = 300
    vim.cmd [[
      highlight CocFloating cterm=reverse guibg=NONE
      highlight! link CocMenuSel PMenuSel
    ]]
  end,

  keys = {
    { "<Tab>", [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]], mode = "i", expr = true, replace_keycodes = false },

    {
      "K",
      function ()
        local cw = vim.fn.expand('<cword>')
        if vim.fn.index({'vim', 'help'}, vim.bo.filetype) >= 0 then
          vim.api.nvim_command('h ' .. cw)
        elseif vim.api.nvim_eval('coc#rpc#ready()') then
          vim.fn.CocActionAsync('doHover')
        else
          vim.api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
        end
      end,
    },

    { "<leader>lD", "<Plug>(coc-declaration)",     desc = "Go To Declaration" },
    { "<leader>ld", "<Plug>(coc-definition)",      desc = "Go To Definition" },
    { "<leader>li", "<Plug>(coc-implementation)",  desc = "Go To Implementation" },
    { "<leader>lr", "<Plug>(coc-references)",      desc = "Go To References" },
    { "<leader>lt", "<Plug>(coc-type-definition)", desc = "Go To Type Definition" },

    { "<leader>;", "<Plug>(coc-format-selected)",    desc = "Format", mode = { "n", "x", }, nowait = true, },
    { "<leader>.", "<Plug>(coc-codeaction-cursor)",  desc = "Action", mode = { "n", "x", }, nowait = true, },
    { "<leader>/", "<Plug>(coc-codeaction-cursor)",  desc = "Rename", mode = { "n", "x", }, nowait = true, },
  },

  opts = {
    colors = { enable = true },
    diagnostic = {
      enableHighlightLineNumber = false,
      enableSign = false,
      floatConfig = { border = true, borderhighlight = "FloatBorder" },
      refreshOnInsertMode = true,
    },
    dialog = { floatBorderHighlight = "FloatBorder" },
    hover = { floatConfig = { border = true, borderhighlight = "FloatBorder" } },
    inlayHint = { enable = false },
    list = {
      alignColumns = true,
      floatPreview = { border = true, borderhighlight = "FloatBorder" },
      statusLineSegments = false,
    },
    notification = { disabledProgressSources = { "*" } },
    semanticTokens = { enable = true },
    signature = { floatConfig = { border = true, borderhighlight = "FloatBorder" } },
    suggest = {
      asciiCharactersOnly = true,
      floatConfig = { border = true, borderhighlight = "FloatBorder" },
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

    -- FIXME: use rc
    Lua = {
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [[/usr/lib/nvim]],
          [[/usr/share/nvim/runtime]],
          [[/usr/share/nvim/runtime/lua/vim/_meta]],
          vim.fs.joinpath (vim.fn.stdpath  [[data]] --[[ @as string]], [[lazy]]),
          vim.fs.joinpath (vim.fn.stdpath  [[data]] --[[ @as string]], [[lazy]], [[lazy.nvim]]),
          vim.fs.joinpath (vim.fn.stdpath  [[data]] --[[ @as string]], [[lazy]], [[neodev.nvim]], [[types]], [[stable]]),
          vim.fs.joinpath (
            vim.fn.stdpath  [[data]] --[[ @as string]],
            [[lazy]],
            [[neodev.nvim]],
            [[types]],
            [[nightly]]
          ),
          vim.api.nvim_get_runtime_file ("", true),
        },
        checkThirdParty = false,
      },
    },
  },

  config = function (opts)
    vim.g.coc_global_extensions = {
      [[coc-marketplace]],
      [[coc-clangd]],
      [[coc-json]],
      [[coc-lua]],
      [[coc-sh]],
      [[coc-xml]],
      [[@statiolake/coc-stylua]],
    }

    for k, v in pairs (opts.opts --[[@as table]]) do
      vim.fn["coc#config"] (k, v)
    end
  end,
}

return Spec
