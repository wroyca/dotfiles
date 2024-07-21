---@type LazyPluginSpec
return {
  [[mini.pairs]],

  keys = {
    { [[`]], mode = [[i]] },
    { [[']], mode = [[i]] },
    { [["]], mode = [[i]] },
    { [[{]], mode = [[i]] },
    { [[}]], mode = [[i]] },
    { [[<]], mode = [[i]] },
    { [[>]], mode = [[i]] },
    { [[(]], mode = [[i]] },
    { [[)]], mode = [[i]] },
    {  "[",  mode = [[i]] },
    {  "]",  mode = [[i]] },
  },

  opts = {
    mappings = {
      ["`"] = { action = [[closeopen]], pair = [[``]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["'"] = { action = [[closeopen]], pair = [['']], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ['"'] = { action = [[closeopen]], pair = [[""]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["{"] = { action = [[closeopen]], pair = [[{}]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["<"] = { action = [[closeopen]], pair = [[<>]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["("] = { action = [[closeopen]], pair = [[()]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["["] = { action = [[closeopen]], pair =  "[]",  neigh_pattern = "[^%S][^%S]", register = { cr = false } }
    }
  }
}
