if vim.bo.filetype ~= "gitconfig" then
	return
end

-- In gitconfig, both # and ; are recognized as comment leaders (characters
-- that mark the start of comments). By default, Neovim prioritize ; as the
-- comment leader, which doesn't align with my preferences where # is the
-- preferred comment leader.
--
vim.opt_local.comments = ":#"
vim.opt_local.commentstring = "# %s"
