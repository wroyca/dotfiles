---@type LazyPluginSpec
return {
  [[mini.hipatterns]],
  dev = true,
  event = [[VeryLazy]],

  init = function()
    require [[mini.hipatterns]].gen_highlighter.hex_color()
  end,

  opts = {
    highlighters = {
      fixme = {
        pattern = '%f[%w]()[Ff][Ii][Xx][Mm][Ee]()%f[%W]:',
        group = [[MiniHipatternsFixme]]
      },
      hack = {
        pattern = '%f[%w]()[Hh][Aa][Cc][Kk]()%f[%W]:',
        group = [[MiniHipatternsHack]]
      },
      todo = {
        pattern = '%f[%w]()[Tt][Oo][Dd][Oo]()%f[%W]:',
        group = [[MiniHipatternsTodo]]
      },
      note = {
        pattern = '%f[%w]()[Nn][Oo][Tt][Ee]()%f[%W]:',
        group = [[MiniHipatternsNote]]
      }
    }
  }
}
