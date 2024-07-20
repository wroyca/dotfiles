if vim.fn.executable [[clang-format]] == 1 then
  vim.opt_local.formatprg = [[clang-format --assume-filename=.json]]
end
