if vim.bo.filetype ~= "gitconfig" then
  return
end

vim.opt_local.comments = ":#"
vim.opt_local.commentstring = "# %s"
