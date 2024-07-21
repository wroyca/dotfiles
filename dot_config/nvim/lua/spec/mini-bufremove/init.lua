return {
  [[mini.bufremove]], name = [[mini-bufremove]], main = [[mini.bufremove]], dev = true,

  opts = {},
  keys = function()
    local bufremove = require([[mini.bufremove]])
    return {
      { [[<leader>bd]], bufremove.delete, mode = [[n]], desc = [[Delete]] },
      { [[<leader>bw]], bufremove.wipeout, mode = [[n]], desc = [[Wipeout]] }
    }
  end
}
