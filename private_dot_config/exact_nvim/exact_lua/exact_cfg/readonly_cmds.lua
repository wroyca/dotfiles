local M = {}
local cmd = require("detail.cmd")

local anchors = {
}

local presets = {
}

function M.setup()
  cmd.load(anchors):map(presets):collect_and_set()
end

return M
