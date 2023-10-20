vim.opt_local.commentstring = "// %s"

vim.api.nvim_create_autocmd({
  [[BufNewFile]],
  [[BufRead]]
}, {
  pattern = {
    "*.cxx",
    "*.hxx",
    "*.ixx",
    "*.txx",
    "*.mxx",
    "*.cxx.in",
    "*.hxx.in",
    "*.ixx.in",
    "*.txx.in",
    "*.mxx.in",
  },
  callback = function()
    vim.cmd [[set filetype=cpp]]
  end
})
