---@type LazyPluginSpec
return {
  [[leap.flit]], config = true,

  keys = function()
    local ret = {}
    for _, key in ipairs({ [[f]], [[F]], [[t]], [[T]] }) do
      ret[#ret + 1] = { key, mode = { [[n]], [[x]], [[o]] }, desc = key }
    end
    return ret
  end
}
