return {
  [[mini.misc]], name = [[mini-misc]], main = [[mini.misc]], dev = true,

  event = [[VeryLazy]],
  config = function()
    local misc = require [[mini.misc]]
    misc.setup()
    misc.setup_auto_root()
    misc.setup_restore_cursor()
  end
}
