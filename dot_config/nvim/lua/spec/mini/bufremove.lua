---@type LazyPluginSpec
return {
  [[mini.clue]], event = [[VeryLazy]],

  opts = {},
  keys = function()
    local bufremove = require([[mini.bufremove]])
    return {
      { [[<leader>bd]], bufremove.delete, mode = [[n]], desc = [[Delete]] },
      { [[<leader>bw]], bufremove.wipeout, mode = [[n]], desc = [[Wipeout]] }
    }
  end
}
