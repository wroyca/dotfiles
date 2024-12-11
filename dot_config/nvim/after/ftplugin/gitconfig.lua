if vim.bo.filetype ~= "gitconfig" then
  return
end

-- Neovim defaults to using ; as the primary comment leader, which does not
-- align with my preference for #.
--
vim.opt_local.comments = ":#"
vim.opt_local.commentstring = "# %s"
