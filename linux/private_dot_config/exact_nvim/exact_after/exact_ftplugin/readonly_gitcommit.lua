vim.api.nvim_create_autocmd({
  [[BufWinEnter]],
}, {
  desc = [[Start commit in insert mode.]],
  callback = function(event)
    if vim.bo.filetype == [[gitcommit]] then
      vim.api.nvim_exec2 ([[startinsert | 1]], {})
      vim.bo[event.buf].buflisted = false
      vim.keymap.set([[n]], [[q]], [[<cmd>q<cr>]], { buffer = event.buf, silent = true })
    end
  end
})
