---@type LazyPluginSpec
local Spec = {
  "mini.hipatterns", dev = true, event = "VeryLazy",

  opts = function ()
    local hipatterns = require  "mini.hipatterns"
    local extra = require  "mini.extra"
    return {
      highlighters = {
        todo = extra.gen_highlighter.words ({ "TODO" }, "MiniHipatternsTodo"),
        note = extra.gen_highlighter.words ({ "NOTE" }, "MiniHipatternsNote"),
        hack = extra.gen_highlighter.words ({ "HACK" }, "MiniHipatternsHack"),

        -- Highlight hex color strings (`#rrggbb`) using that color
        hex_color = hipatterns.gen_highlighter.hex_color (),
      },
    }
  end,
}

return Spec
