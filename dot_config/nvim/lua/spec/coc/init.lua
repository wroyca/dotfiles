---@type LazyPluginSpec
return {
  [[neoclide/coc.nvim]], event = [[VeryLazy]], branch = [[release]],

  init = function()
    vim.o.updatetime = 300
    vim.cmd.hi [[CocFloating cterm=reverse                               guibg=NONE]]
    vim.cmd.hi [[CocMenuSel  cterm=underline,reverse guifg=NvimDarkGrey3 guibg=NvimLightGrey2 blend=0]]
  end,

  keys = {
    { [[<Tab>]], [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]], mode = [[i]], expr = true, replace_keycodes = false },

    {
      [[K]],
      function()
        local cw = vim.fn.expand([[<cword>]])
        if vim.fn.index({[[vim]], [[help]]}, vim.bo.filetype) >= 0 then
          vim.api.nvim_command([[h ]] .. cw)
        elseif vim.api.nvim_eval([[coc#rpc#ready()]]) then
          vim.fn.CocActionAsync([[doHover]])
        else
          vim.api.nvim_command([[!]] .. vim.o.keywordprg .. [[ ]] .. cw)
        end
      end
    },

    { [[gD]], [[<Plug>(coc-declaration)]],     desc = [[Go to declaration]]     },
    { [[gd]], [[<Plug>(coc-definition)]],      desc = [[Go to definition]]      },
    { [[gi]], [[<Plug>(coc-implementation)]],  desc = [[Go to implementation]]  },
    { [[gr]], [[<Plug>(coc-references)]],      desc = [[Go to references]]      },
    { [[gt]], [[<Plug>(coc-type-definition)]], desc = [[Go to type definition]] },

    {
      [[<leader>;]],
      [[<Plug>(coc-format-selected)]],
      desc = [[Format selection]],
      mode = {
        [[n]],
        [[x]]
      },
      nowait = true
    },

    {
      [[<leader>.]],
      [[<Plug>(coc-codeaction-cursor)]],
      desc = [[Code action]],
      mode = {
        [[n]],
        [[x]]
      },
      nowait = true
    },

    { [[<leader>lo]], [[<cmd>CocOutline<cr>]],    desc = [[Outline]],    silent = true },
    { [[<leader>ld]], [[<cmd>CocDiagnostic<cr>]], desc = [[Diagnostic]], silent = true },
  },

  opts = {
    ------------------------------------------------------------------------------
    -- Colors
    ------------------------------------------------------------------------------
    ["colors.enable"] = true,

    ------------------------------------------------------------------------------
    -- Diagnostic
    ------------------------------------------------------------------------------
    ["diagnostic.enableHighlightLineNumber"] = false,
    ["diagnostic.enableSign"] = false,
    ["diagnostic.floatConfig"] = {
      border = true,
      borderhighlight = [[FloatBorder]]
    },
    ["diagnostic.refreshOnInsertMode"] = true,

    ------------------------------------------------------------------------------
    -- Dialog
    ------------------------------------------------------------------------------
    ["dialog.floatBorderHighlight"] = "FloatBorder",

    ------------------------------------------------------------------------------
    -- Hover
    ------------------------------------------------------------------------------
    ["hover.floatConfig"] = {
      border = true,
      borderhighlight = [[FloatBorder]]
    },

    ------------------------------------------------------------------------------
    -- InlayHint
    ------------------------------------------------------------------------------
    ["inlayHint.enable"] = false,

    ------------------------------------------------------------------------------
    -- List
    ------------------------------------------------------------------------------
    ["list.alignColumns"] = true,
    ["list.floatPreview"] = {
      border = true,
      borderhighlight = [[FloatBorder]]
    },
    ["list.statusLineSegments"] = nil,

    ------------------------------------------------------------------------------
    -- Notification
    ------------------------------------------------------------------------------
    ["notification.disabledProgressSources"] = {
      "*"
    },

    ------------------------------------------------------------------------------
    -- SemanticTokens
    ------------------------------------------------------------------------------
    ["semanticTokens.enable"] = true,

    ------------------------------------------------------------------------------
    -- Signature
    ------------------------------------------------------------------------------
    ["signature.floatConfig"] = {
      border = true,
      borderhighlight = [[FloatBorder]]
    },

    ------------------------------------------------------------------------------
    -- Suggest
    ------------------------------------------------------------------------------
    ["suggest.asciiCharactersOnly"] = true,
    ["suggest.floatConfig"] = {
      border = true,
      borderhighlight = [[FloatBorder]]
    },
    ["suggest.removeDuplicateItems"] = true,

    ------------------------------------------------------------------------------
    -- Preferences
    ------------------------------------------------------------------------------
    ["coc.preferences.currentFunctionSymbolAutoUpdate"] = true,
    ["coc.preferences.enableLinkedEditing"] = true,
    ["coc.preferences.formatOnType"] = true,

    ------------------------------------------------------------------------------
    -- Language Server
    ------------------------------------------------------------------------------
    ["clangd.arguments"] = {
      [[--all-scopes-completion=true]],
      [[--background-index-priority=normal]],
      [[--background-index=true]],
      [[--clang-tidy]],
      [[--completion-parse=always]],
      [[--completion-style=bundled]],
      [[--function-arg-placeholders=false]],
      [[--header-insertion=never]],
      [[--parse-forwarding-functions]],
      [[--pch-storage=memory]],
      [[--ranking-model=decision_forest]]
    },

    ["Lua.workspace.checkThirdParty"] = false,
    ["Lua.workspace.library"] = {
      [[/usr/lib/nvim]],
      [[/usr/share/nvim/runtime]],
      [[/usr/share/nvim/runtime/lua/vim/_meta]],

      vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string]], [[lazy]]),
      vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string]], [[lazy]], [[lazy.nvim]]),
      vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string]], [[lazy]], [[neodev.nvim]], [[types]], [[stable]]),
      vim.fs.joinpath(vim.fn.stdpath [[data]] --[[ @as string]], [[lazy]], [[neodev.nvim]], [[types]], [[nightly]])
    }
  },

  config = function(opts)
    vim.g.coc_global_extensions = {
      [[coc-marketplace]],

      [[coc-clangd]],
      [[coc-json]],
      [[coc-lua]],
      [[coc-sh]],
      [[coc-xml]],

      [[@statiolake/coc-stylua]],
    }

    for k, v in pairs(opts.opts --[[@as table]]) do
      vim.fn["coc#config"](k, v)
    end
  end
}
