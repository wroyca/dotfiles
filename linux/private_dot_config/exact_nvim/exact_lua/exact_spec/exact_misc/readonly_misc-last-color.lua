---@type LazyPluginSpec
return {
  [[misc-last-color]],
  lazy = false,
  config = function()
    local s, _ = pcall(vim.api.nvim_exec2, (([[colorscheme %s]]):format(
      require([[last-color]]).recall()
    )), {})
    if not s then s, _ = pcall(vim.api.nvim_exec2, ([[colorscheme randomhue]]), {})
      if not s then pcall(vim.api.nvim_exec2, ([[colorscheme default]]), {}) end
    end
  end
}
