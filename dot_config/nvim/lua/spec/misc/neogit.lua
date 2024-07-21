---@type LazyPluginSpec
return {
  [[neogit]], dependencies = { [[neogit-diffview]], [[neogit-telescope]] },

  keys = {
    { [[<leader>gn]], [[<cmd>Neogit<cr>]], desc = [[Neogit]] }
  },

  opts = {
    auto_show_console = false,
    disable_signs = true,
    graph_style = [[unicode]],
    integrations = {
      diffview = true,
      telescope = true
    }
  },

  config = function(_, opts)
    for _, e in ipairs({
      [[commit_editor]],
      [[commit_select_view]],
      [[commit_view]],
      [[log_view]],
      [[merge_editor]],
      [[popup]],
      [[preview_buffer]],
      [[rebase_editor]],
      [[reflog_view]],
      [[tag_editor]]
    }) do
      opts[e] = { kind = [[tab]] }
    end
    require [[neogit]].setup(opts)
  end
}
