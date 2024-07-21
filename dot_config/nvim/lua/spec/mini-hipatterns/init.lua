return {
  [[mini.hipatterns]], name = [[mini-hipatterns]], main = [[mini.hipatterns]], dev = true,

  event = [[VeryLazy]],
  opts = function()
    local hipatterns = require [[mini.hipatterns]]
    local extra = require [[mini.extra]]
    return {
      highlighters = {
        todo = extra.gen_highlighter.words({ [[TODO]] }, [[MiniHipatternsTodo]]),
        note = extra.gen_highlighter.words({ [[NOTE]] }, [[MiniHipatternsNote]]),

        -- Highlight hex color strings (`#rrggbb`) using that color
        hex_color = hipatterns.gen_highlighter.hex_color(),
      }
    }
  end
}
