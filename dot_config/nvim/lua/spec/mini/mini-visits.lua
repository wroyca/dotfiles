---@type LazyPluginSpec
return {
  [[mini.visits]],
  dev = true,
  config = true,
  keys = {
    { [[<leader>vl]],  function() require [[mini.extra]].pickers.visit_labels() end, desc = [[Labels]] },
    { [[<leader>vp]],  function() require [[mini.extra]].pickers.visit_paths() end,  desc = [[Paths]] },
    { [[<leader>vap]], function() require [[mini.visits]].add_path() end,            desc = [[Path]] },
    { [[<leader>val]], function() require [[mini.visits]].add_label() end,           desc = [[Label]] }
  }
}
