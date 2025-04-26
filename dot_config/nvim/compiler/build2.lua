if vim.b.current_compiler then
  return
end

-- https://github.com/neovim/neovim/issues/1496

vim.b.current_compiler = "b"

vim.o.makeprg = "b"
