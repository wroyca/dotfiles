---@type LazyPluginSpec
return {
  [[mini.hues]],
  dev = true,
  lazy = false,
  priority = 1000,
  config = function()
    vim.api.nvim_exec2 ([[colorscheme randomhue]], {})
  end
}
