---@type LazyPluginSpec
return {
  [[misc-neogit]],
  dependencies = {
    { name = [[misc-neogit-telescope]], [[nvim-telescope/telescope.nvim]] },
    { name = [[misc-neogit-diffview]],  [[sindrets/diffview.nvim]] }
  },

  keys = {
    { [[<leader>gn]], [[<cmd>Neogit<cr>]], desc = [[Neogit]] },
    { [[<leader>gN]],
      function()
        -- https://github.com/NeogitOrg/neogit/discussions/899
        local line = vim.api.nvim_win_get_cursor(0)[1]
        local line_range = line .. [[,]] .. line
        local annotation = vim.fn.systemlist([[git annotate -M --porcelain ]] .. vim.fn.expand [[%:p]] .. [[ -L]] .. line_range)[1]

        if vim.v.shell_error ~= 0 then
          return vim.notify(annotation, vim.log.levels.WARN)
        end

        local ref = vim.split(annotation, [[ ]])[1]
        if ref == [[0000000000000000000000000000000000000000]] then
          return vim.notify([[There's no one to take the blame]], vim.log.levels.WARN)
        end

        local commit_view = require [[neogit.buffers.commit_view]].new(ref, false)
        commit_view:open()
      end,
      desc = [[Neogit Blame]]
    }
  },

  opts = {
    commit_editor      = { kind = [[tab]] },
    commit_select_view = { kind = [[tab]] },
    commit_view        = { kind = [[tab]] },
    log_view           = { kind = [[tab]] },
    rebase_editor      = { kind = [[tab]] },
    reflog_view        = { kind = [[tab]] },
    merge_editor       = { kind = [[tab]] },
    tag_editor         = { kind = [[tab]] },
    preview_buffer     = { kind = [[tab]] },
    popup              = { kind = [[tab]] },

    -- Experimental
    --
    graph_style        = "unicode",
  }
}
