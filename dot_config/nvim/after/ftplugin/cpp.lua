if vim.fn.executable [[clang-format]] == 1 then
  vim.opt_local.formatprg = [[clang-format --assume-filename=.cpp]]
end

vim.opt_local.cinkeys:remove [[:]]
vim.opt_local.textwidth = 120
vim.opt_local.colorcolumn = [[121]]
