---@type LazyPluginSpec
return {
  [[misc-neogit]],
  cmd = [[Neogit]],
  keys = { { [[<leader>gn]], [[<cmd>Neogit<cr>]], desc = [[Neogit]] } },
  opts = {
    graph_style = [[unicode]],
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
      [[popup]],
    }) do
      opts[e] = { kind = [[tab]] }
    end
    require([[neogit]]).setup(opts)
  end,
}
