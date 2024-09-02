if vim.bo.filetype ~= "cpp" then return end
if vim.fn.executable "clang-format" == 1 then
  vim.opt_local.formatprg = "clang-format --assume-filename=.cpp"
end

vim.bo.commentstring = '// %s'
vim.opt_local.cinkeys:remove ":"
