---@type LazyPluginSpec
return {
  [[coc]],
  event = [[VeryLazy]],

  init = function()
    vim.o.updatetime = 300
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
    { [[gD]], [[<Plug>(coc-declaration)]], desc = [[Go to declaration]] },
    { [[gd]], [[<Plug>(coc-definition)]], desc = [[Go to definition]] },
    { [[gi]], [[<Plug>(coc-implementation)]], desc = [[Go to implementation]] },
    { [[gr]], [[<Plug>(coc-references)]], desc = [[Go to references]] },
    { [[gt]], [[<Plug>(coc-type-definition)]], desc = [[Go to type definition]] },
    {
      [[<leader>;]],
      [[<Plug>(coc-format-selected)]],
      desc = [[Format selection]],
      mode = { [[n]], [[x]] }
    },
    {
      [[<leader>.]],
      [[<Plug>(coc-codeaction-cursor)]],
      desc = [[Code action]],
      nowait = true
    },
    { [[<leader>co]], [[<cmd>CocOutline<cr>]], desc = [[Outline]], silent = true },
    { [[<leader>cd]], [[<cmd>CocDiagnostic<cr>]], desc = [[Diagnostic]], silent = true },
    { [[<leader>cs]], [[<cmd>CocList symbols<cr>]], desc = [[Symbols]], silent = true },
    { [[<leader>cl]], [[<cmd>CocList links<cr>]], desc = [[Links]], silent = true },
    { [[<leader>cc]], [[<cmd>CocList commands<cr>]], desc = [[Commands]], silent = true },
    { [[<leader>cL]], [[<cmd>CocList lists<cr>]], desc = [[Lists]], silent = true }
  },

  opts = {
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
      [[--background-index=true]],
      [[--background-index-priority=normal]],
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

      ---@diagnostic disable-next-line: param-type-mismatch
      vim.fs.joinpath(vim.fn.stdpath [[data]], [[lazy]], [[lazy.nvim]]),
      ---@diagnostic disable-next-line: param-type-mismatch
      vim.fs.joinpath(vim.fn.stdpath [[data]], [[lazy]], [[neodev.nvim]], [[types]], [[nightly]])
    }
  },

  config = function(opts)
    vim.g.coc_global_extensions = {
      [[coc-marketplace]],
      [[coc-lua]],
      [[@statiolake/coc-stylua]],
      [[coc-clangd]]
    }

    ---@diagnostic disable-next-line: param-type-mismatch
    for k, v in pairs(opts.opts) do
      vim.fn["coc#config"](k, v)
    end
  end
}