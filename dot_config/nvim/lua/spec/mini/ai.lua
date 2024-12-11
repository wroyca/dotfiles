---@module "mini.ai"

---@type LazyPluginSpec
local Spec = {
  "mini.ai", virtual = true,

  keys = {
    { "a", mode = { "x", "o" } },
    { "i", mode = { "x", "o" } },
  },

  opts = function ()
    local gen_ai_spec = require ("mini.extra").gen_ai_spec
    return {
      custom_textobjects = {
        B = gen_ai_spec.buffer (),
      },
    }
  end,
}

return Spec
