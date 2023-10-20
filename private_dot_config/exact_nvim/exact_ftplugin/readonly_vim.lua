-- Disable erroneous $VIMRUNTIME/syntax/lua.vim
-- https://github.com/neovim/neovim/issues/20456
--
vim.b.ts_highlight = 1

-- Use treesitter highlight for vimscripts.
--
vim.treesitter.start()
