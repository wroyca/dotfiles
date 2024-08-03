---@type LazyPluginSpec
local Spec = {
  "mini.ai", dev = true,

  keys = {
    { [[a]], mode = { [[x]], [[o]] } },
    { [[i]], mode = { [[x]], [[o]] } }
  },

  opts = function()
    local gen_ai_spec = require "mini.extra".gen_ai_spec
    return {
      custom_textobjects = {
        B = gen_ai_spec.buffer(),
        D = gen_ai_spec.diagnostic(),
        I = gen_ai_spec.indent(),
        L = gen_ai_spec.line(),
        N = gen_ai_spec.number()
      }
    }
  end
}

return Spec
