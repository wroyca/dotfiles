---@type LazyPluginSpec
return {
  [[mini.diff]], event = [[VeryLazy]],

  init = function()
    vim.o.signcolumn = [[yes:1]]
  end,

  opts = {
    delay = {
      text_change = 0
    },
    view = {
      style = [[sign]],
      signs = { add = [[┃]], change = [[┃]], delete = [[┃]] }
    }
  }
}
