if vim.bo.filetype ~= "gitconfig" then
  return
end

-- The `#` and `;` characters begin comments. By default, Neovim will use `;`,
-- but we prefer to use `#`.
--
vim.opt_local.comments = ":#"
vim.opt_local.commentstring = "# %s"
