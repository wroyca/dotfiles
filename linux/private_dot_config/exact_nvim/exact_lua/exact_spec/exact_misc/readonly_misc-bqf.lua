---@type LazyPluginSpec
return {
  [[misc-bqf]],
  ft = [[qf]],
  dependencies = {
    {
      [[junegunn/fzf]],
      name = [[misc-fzf]],
      build = [[:call fzf#install()]],
    },
  },
  opts = {
    preview = {
      auto_preview = {
        default = false,
      },
    },
  },
}
