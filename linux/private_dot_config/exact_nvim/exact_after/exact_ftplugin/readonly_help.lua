vim.api.nvim_create_autocmd({
  [[BufWinEnter]],
}, {
  desc = [[Use vertical split for help pages.]],
  callback = function(event)
    if vim.bo.filetype == [[help]] then
      vim.api.nvim_exec2 ([[wincmd L]], {})
      vim.bo[event.buf].buflisted = false
      vim.keymap.set([[n]], [[q]], [[<cmd>q<cr>]], { buffer = event.buf, silent = true })
    end
  end
})
