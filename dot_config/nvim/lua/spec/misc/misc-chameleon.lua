---@type LazyPluginSpec
return {
  [[misc-chameleon]],
  event = [[VeryLazy]],
  config = function()
    require [[chameleon]].setup()
  end
}
