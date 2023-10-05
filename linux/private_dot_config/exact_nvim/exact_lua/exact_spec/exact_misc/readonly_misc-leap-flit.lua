---@type LazyPluginSpec
return {
  [[misc-leap-flit]],
  keys = function()
    ret = {}
    for _, key in ipairs({ [[f]], [[F]], [[t]], [[T]] }) do
      ret[#ret + 1] = { key, mode = { [[n]], [[x]], [[o]] }, desc = key }
    end
    return ret
  end,
  config = true,
}
