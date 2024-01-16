---@type LazyPluginSpec
return {
  [[mini.hues]],
  dev = true,
  lazy = false,
  config = function()
    vim.api.nvim_exec2 ([[colorscheme randomhue]], {})
  end
}
