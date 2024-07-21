---@type LazyPluginSpec
return {
  [[mini.visits]], event = [[VeryLazy]],

  keys = function()
    local extra = require [[mini.extra]]
    return {
      { [[<leader>vp]], extra.pickers.visit_paths,  desc = [[Paths]]  },
      { [[<leader>vl]], extra.pickers.visit_labels, desc = [[Labels]] },
    }
  end,

  opts = {
    list = {
      filter = function(item)
        return not item.path:match('^%.git/')
      end
    }
  }
}
