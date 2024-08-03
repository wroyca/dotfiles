---@type LazyPluginSpec
local Spec = {
  "mini.align", dev = true, config = true,
  keys = {
    { [[gA]], mode = { [[n]], [[x]] }, desc = [[Align with preview]] },
    { [[ga]], mode = { [[n]], [[x]] }, desc = [[Align]] }
  }
}

return Spec
