---@type LazyPluginSpec
return {
  [[misc-leap]],
  keys = { [[s]], [[S]], [[gs]] },
  opts = {
    -- The idea here is that visual indicator will helps us anticipate whether
    -- we'll need to input an additional label. That is, underlined targets are
    -- guaranteed to be invariant at 2 characters, while non-underlined targets
    -- will always involve 2 on-screen characters plus a label.
    --
    highlight_unlabeled_phase_one_targets = true,
  },
  config = function(_, opts)
    require [[leap]].setup(opts)
    require [[leap]].add_default_mappings(true)

    -- https://github.com/neovim/neovim/issues/20793
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
