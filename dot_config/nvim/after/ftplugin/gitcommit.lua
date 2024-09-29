if vim.bo.filetype ~= "gitcommit" then
  return
end

vim.opt_local.smartindent = false
vim.opt_local.textwidth = 72
vim.opt_local.formatoptions:remove({ "c", "r", "o", "q" })
