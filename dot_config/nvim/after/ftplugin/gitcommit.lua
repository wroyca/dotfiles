if vim.bo.filetype ~= "gitcommit" then
  return
end

vim.opt_local.textwidth = 72
vim.opt_local.formatoptions:remove({ "c", "r", "o", "q" })

-- Smartindent can occasionally interfere with paragraph formatting in specific
-- situations. For instance, when editing plain text rather than code, it might
-- automatically insert unintended indentation.
--
vim.opt_local.smartindent = false
