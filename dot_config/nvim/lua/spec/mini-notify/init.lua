return {
  [[mini.notify]], name = [[mini-notify]], main = [[mini.notify]], dev = true,

  event = [[VeryLazy]],
  config = function(_, opts)
    vim.notify = require [[mini.notify]].make_notify({})
  end
}
