if vim.bo.filetype ~= "gitcommit" then
  return
end

-- The smartindent option can sometimes disrupt paragraph formatting in certain
-- situations. For example, when editing text in a way that doesn't involve
-- code, smartindent may insert unexpected indentation, breaking paragraph
-- intended alignment.
--
vim.opt_local.smartindent = false

vim.opt_local.textwidth = 72
vim.opt_local.formatoptions:remove({ "c", "r", "o", "q" })
