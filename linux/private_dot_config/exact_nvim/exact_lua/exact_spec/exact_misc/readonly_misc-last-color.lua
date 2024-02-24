---@type LazyPluginSpec
return {
  [[misc-last-color]],
  lazy = false,
  config = function()
    -- stylua: ignore
    pcall(vim.api.nvim_exec2, (([[colorscheme %s]]):format(
      require([[last-color]]).recall()
    )), {})
  end,
}
