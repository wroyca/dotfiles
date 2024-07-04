---@type LazyPluginSpec
return {
  [[leap]],

  keys = { [[s]], [[S]], [[gs]] },
  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },

  config = function(_, opts)
    require [[leap]].setup(opts)
    require [[leap]].add_default_mappings(true)

    -- Leap is pinned to an older version due to recent changes breaking my
    -- workflows. Thus, see https://github.com/ggandor/leap.nvim/issues/70
    --
    vim.api.nvim_create_autocmd([[user]], {
      pattern = [[LeapEnter]],
      callback = function()
        vim.cmd.hi([[Cursor]], [[blend=100]])
        vim.opt.guicursor:append({ [[a:Cursor/lCursor]] })
      end,
    })
    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[LeapLeave]],
      callback = function()
        vim.cmd.hi([[Cursor]], [[blend=0]])
        vim.opt.guicursor:remove({ [[a:Cursor/lCursor]] })
      end
    })
  end
}
