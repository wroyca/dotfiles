---@type LazyPluginSpec
return {
  [[mini.hipatterns]],
  event = [[VeryLazy]],
  opts = {
    highlighters = {
      fixme = {
        pattern = "%f[%w]()[Ff][Ii][Xx][Mm][Ee]()%f[%W]:",
        group = [[MiniHipatternsFixme]]
      },
      hack = {
        pattern = "%f[%w]()[Hh][Aa][Cc][Kk]()%f[%W]:",
        group = [[MiniHipatternsHack]]
      },
      todo = {
        pattern = "%f[%w]()[Tt][Oo][Dd][Oo]()%f[%W]:",
        group = [[MiniHipatternsTodo]]
      },
      note = {
        pattern = "%f[%w]()[Nn][Oo][Tt][Ee]()%f[%W]:",
        group = [[MiniHipatternsNote]]
      }
    }
  },
  config = function(_, opts)
    local hipatterns = require [[mini.hipatterns]]
    hipatterns.setup(opts)
    hipatterns.gen_highlighter.hex_color()
  end
}
