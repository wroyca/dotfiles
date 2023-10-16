local M = {}
local key = require "detail.key"

local anchors = {
}

local presets = {
}

function M.setup()
  key.load(anchors):map(presets):collect_and_set()
end

return M
