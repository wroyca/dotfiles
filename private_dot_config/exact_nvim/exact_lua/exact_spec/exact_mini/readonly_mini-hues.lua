---@type LazyPluginSpec
return {
  [[echasnovski/mini.hues]],
  lazy = false,
  priority = 1000,

  config = function()
    -- single-color foreground; see discussions at:
    -- https://github.com/echasnovski/mini.nvim/issues/538
    vim.api.nvim_create_autocmd("colorscheme", {
      pattern = "*",
      callback = function()
        vim.cmd [[highlight! link DiffAdd    DiagnosticOk]]
        vim.cmd [[highlight! link DiffChange DiagnosticWarn]]
        vim.cmd [[highlight! link DiffDelete DiagnosticError]]
      end
    })

    vim.cmd [[colorscheme randomhue]]
  end
}
