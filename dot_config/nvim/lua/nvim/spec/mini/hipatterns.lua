---@module "mini.hipatterns"

---@type LazyPluginSpec
local Spec = {
  "mini.hipatterns", dev = true, event = "VeryLazy",

  opts = function()
    local extra = require("mini.extra")
    return {
      highlighters = {
        fixme = extra.gen_highlighter.words({ "FIXME", "Fixme", "fixme" }, "MiniHipatternsFixme"),
        hack = extra.gen_highlighter.words({ "HACK", "Hack", "hack" }, "MiniHipatternsHack"),
        note = extra.gen_highlighter.words({ "NOTE", "Note", "note" }, "MiniHipatternsNote"),
        todo = extra.gen_highlighter.words({ "TODO", "Todo", "todo" }, "MiniHipatternsTodo"),
      },
    }
  end,
}

return Spec
