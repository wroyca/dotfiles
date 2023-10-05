---@type LazyPluginSpec
return {
  [[mini.pairs]],
  event = [[VeryLazy]],
  opts = {
    mappings = {
      ["`"] = { action = [[closeopen]], pair = [[``]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["'"] = { action = [[closeopen]], pair = [['']], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ['"'] = { action = [[closeopen]], pair = [[""]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["{"] = { action = [[closeopen]], pair = [[{}]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["<"] = { action = [[closeopen]], pair = [[<>]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["("] = { action = [[closeopen]], pair = [[()]], neigh_pattern = "[^%S][^%S]", register = { cr = false } },
      ["["] = { action = [[closeopen]], pair = "[]", neigh_pattern = "[^%S][^%S]", register = { cr = false } },
    },
  },
}
