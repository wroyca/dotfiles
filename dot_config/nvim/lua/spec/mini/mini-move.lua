---@type LazyPluginSpec
return {
  [[mini.move]],
  dev = true,
  config = true,
  keys = {
    { [[<A-h>]], mode = [[n]], desc = [[Move line left]] },
    { [[<A-j>]], mode = [[n]], desc = [[Move line down]] },
    { [[<A-k>]], mode = [[n]], desc = [[Move line up]] },
    { [[<A-l>]], mode = [[n]], desc = [[Move line right]] },
    { [[<A-h>]], mode = [[v]], desc = [[Move selection left]] },
    { [[<A-j>]], mode = [[v]], desc = [[Move selection down]] },
    { [[<A-k>]], mode = [[v]], desc = [[Move selection up]] },
    { [[<A-l>]], mode = [[v]], desc = [[Move selection right]] }
  }
}
