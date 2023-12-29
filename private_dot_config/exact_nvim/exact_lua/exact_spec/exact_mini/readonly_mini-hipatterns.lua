---@type LazyPluginSpec
return {
  [[mini.hipatterns]],
  dev = true,
  event = [[VeryLazy]],

  config = function()
    local hipatterns = require [[mini.hipatterns]]
    hipatterns.setup({
      highlighters = {
        -- Highlight standalone 'FIXME:', 'HACK:', 'TODO:', 'NOTE:'
        fixme     = { pattern = '%f[%w]()[Ff][Ii][Xx][Mm][Ee]()%f[%W]:', group = [[MiniHipatternsFixme]] },
        hack      = { pattern = '%f[%w]()[Hh][Aa][Cc][Kk]()%f[%W]:', group = [[MiniHipatternsHack]] },
        todo      = { pattern = '%f[%w]()[Tt][Oo][Dd][Oo]()%f[%W]:', group = [[MiniHipatternsTodo]] },
        note      = { pattern = '%f[%w]()[Nn][Oo][Tt][Ee]()%f[%W]:', group = [[MiniHipatternsNote]] },

        hex_color = hipatterns.gen_highlighter.hex_color()
      }
    })
  end
}
