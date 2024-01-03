vim.opt_local.commentstring = [[// %s]]

vim.api.nvim_create_autocmd({
  [[BufNewFile]],
  [[BufRead]]
}, {
  pattern = {
    "*.cxx",
    "*.hxx",
    "*.ixx",
    "*.txx",
    "*.mxx"
  },
  callback = function()
    vim.api.nvim_exec2 ([[set filetype=cpp]], {})
  end
})
