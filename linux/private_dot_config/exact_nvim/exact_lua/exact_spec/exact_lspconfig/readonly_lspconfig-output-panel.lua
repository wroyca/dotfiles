---@type LazyPluginSpec
return {
  [[lspconfig-output-panel]],
  config = true,
  cmd = [[OutputPanel]],
  keys = {
    { [[<leader>lo]],  [[<cmd>:OutputPanel<cr>]], mode = [[n]], desc = [[Output panel]] },
  }
}
