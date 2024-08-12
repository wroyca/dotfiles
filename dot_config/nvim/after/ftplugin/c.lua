vim.opt_local.path = [[/usr/include/**,/usr/local/include/**,/lib/clang/**]]

if vim.bo.filetype ~= "c" then return end
if vim.fn.executable [[clang-format]] == 1 then
  vim.opt_local.formatprg = [[clang-format --assume-filename=.c]]
end
