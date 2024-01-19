---@type LazyPluginSpec
return {
  [[misc-bqf]],
  event = [[VeryLazy]],
  config = true,
  dependencies = {
    { name = [[misc-fzf]], [[junegunn/fzf]], build = [[:call fzf#install()]] }
  }
}
