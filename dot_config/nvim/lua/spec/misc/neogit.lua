---@type LazyPluginSpec
return {
  [[neogit]],
  dependencies = {
    { name = [[misc-neogit-telescope]], [[nvim-telescope/telescope.nvim]] },
    { name = [[misc-neogit-diffview]],  [[sindrets/diffview.nvim]] },
  },

  keys = { { [[<leader>gn]], [[<cmd>Neogit<cr>]], desc = [[Neogit]] } },
  opts = {
    graph_style = [[unicode]],
    auto_show_console = false,
    disable_signs = true,
    integrations = {
      telescope = true,
      diffview = true
    }
  },

  config = function(_, opts)
    for _, e in ipairs({
      [[commit_editor]],
      [[commit_select_view]],
      [[commit_view]],
      [[log_view]],
      [[rebase_editor]],
      [[reflog_view]],
      [[merge_editor]],
      [[tag_editor]],
      [[preview_buffer]],
      [[popup]]
    }) do
      opts[e] = { kind = [[tab]] }
    end
    require [[neogit]].setup(opts)
  end
}

