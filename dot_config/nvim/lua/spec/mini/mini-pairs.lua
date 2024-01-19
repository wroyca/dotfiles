---@type LazyPluginSpec
return {
  [[mini.pairs]],
  enabled = false,
  dev = true,
  event = [[VeryLazy]],

  -- While I'm not particularly fond of the entire traversal aspect, other
  -- autopairs plugins aren't as good, so let's bear with it for now and
  -- perhaps it will grows on me.
  --
  opts = {
    mappings = {
      ['`'] = { action = 'closeopen', pair = [[``]], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ["'"] = { action = 'closeopen', pair = [['']], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ['"'] = { action = 'closeopen', pair = [[""]], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ['{'] = { action = 'closeopen', pair = [[{}]], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ['<'] = { action = 'closeopen', pair = [[<>]], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ['('] = { action = 'closeopen', pair = [[()]], neigh_pattern = '[^%S][^%S]', register = { cr = false } },
      ['['] = { action = 'closeopen', pair =  '[]',  neigh_pattern = '[^%S][^%S]', register = { cr = false } },
    }
  }
}
