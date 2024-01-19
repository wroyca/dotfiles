---@type LazyPluginSpec
return {
  [[misc-auto-dark-mode]],
  event = [[VeryLazy]],
  opts = {
    set_dark_mode = function()
      vim.o.background = [[dark]]
    end,
    set_light_mode = function()
      vim.o.background = [[light]]
    end
  }
}
