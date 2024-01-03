---@type LazyPluginSpec
return {
  [[tzachar/fuzzy.nvim]],
  name = [[cmp-fuzzy]],
  event = [[CmdLineEnter]],
  dependencies = {
    [[tzachar/cmp-fuzzy-path]],
    [[telescope-fzf-native]]
  }
}
