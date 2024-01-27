---@type LazyPluginSpec
return {
  [[lspconfig-rulebook]],
  event = [[VeryLazy]],
  config = true,
  keys = {
    { [[<leader>lLl]],  function() require [[rulebook]].ignoreRule() end,  mode = [[n]], desc = [[Lookup]] },
    { [[<leader>lLi]],  function() require [[rulebook]].ignoreRule() end,  mode = [[n]], desc = [[Ignore]] },
  }
}
