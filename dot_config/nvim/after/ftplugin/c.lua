vim.opt_local.path = [[/usr/include/**,/usr/local/include/**,/lib/clang/**]]
vim.opt_local.makeprg = [[b]]

if vim.bo.filetype ~= "c" then return end
if vim.fn.executable [[clang-format]] == 1 then
  vim.opt_local.formatprg = [[clang-format --assume-filename=.c]]
end

vim.opt_local.textwidth = 80
vim.opt_local.colorcolumn = [[81]]
