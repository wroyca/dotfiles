---@diagnostic disable: undefined-field, missing-fields, assign-type-mismatch
---@type LazyPluginSpec
return {
  [[ggandor/leap.nvim]],
  name = [[leap]],

  keys = {
    { [[s]],  mode = { [[n]], [[x]], [[o]] }, desc = [[Leap forward to]] },
    { [[S]],  mode = { [[n]], [[x]], [[o]] }, desc = [[Leap backward to]] },
    { [[gs]], mode = { [[n]], [[x]], [[o]] }, desc = [[Leap from windows]] }
  },

  opts = {
    -- Visual indicator to helps us anticipate whether we'll need to input an
    -- additional label (a third character). That is, underlined targets are
    -- guaranteed to be invariant at 2 characters, while non-underlined targets
    -- will always involve 2 on-screen characters plus a label.
    --
    highlight_unlabeled_phase_one_targets = true
  },

  config = function(_, opts)
    local leap = require [[leap]]
    leap.setup(opts)
    leap.add_default_mappings(true)

    vim.api.nvim_create_autocmd([[user]], {
      pattern = [[LeapEnter]],
      callback = function()
        vim.cmd.hi([[Cursor]], [[blend=100]])
        vim.opt.guicursor:append { [[a:Cursor/lCursor]] }
      end,
    })

    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[LeapLeave]],
      callback = function()
        vim.cmd.hi([[Cursor]], [[blend=0]])
        vim.opt.guicursor:remove { [[a:Cursor/lCursor]] }
      end
    })

    vim.api.nvim_create_autocmd([[ColorScheme]], {
      callback = function ()
        vim.api.nvim_set_hl(0, [[LeapBackdrop]], { link = [[Comment]] })
      end
    })
  end
}
