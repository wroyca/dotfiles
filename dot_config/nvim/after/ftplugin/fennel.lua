if vim.fn.executable [[fnlfmt]] == 1 then
  vim.opt_local.formatprg = [[fnlfmt /dev/stdin]]
end
