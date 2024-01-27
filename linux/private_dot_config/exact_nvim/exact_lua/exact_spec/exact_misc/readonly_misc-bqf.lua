---@type LazyPluginSpec
return {
  [[misc-bqf]],
  event = [[VeryLazy]],
  dependencies = {
    { 
      name = [[misc-fzf]], [[junegunn/fzf]], 
      build = [[:call fzf#install()]] 
    }
  },
  
  opts = {
    preview = {
      auto_preview = {
        default = false
      }
    }
  }
}
