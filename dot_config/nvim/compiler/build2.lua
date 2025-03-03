-- Vim compiler file
-- Compiler: build2

if vim.b.current_compiler then
  return
end

vim.b.current_compiler = 'b'
vim.o.makeprg = 'b'
