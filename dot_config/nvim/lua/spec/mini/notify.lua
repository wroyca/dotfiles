---@type LazyPluginSpec
return {
  [[mini.notify]], event = [[VeryLazy]],

  opts = {
    ERROR = {
      duration = 10000
    }
  },

  config = function(_, opts)
    vim.notify = require [[mini.notify]].make_notify(opts)
  end
}
