if vim.bo.filetype ~= "help" then return end
vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function(event)
    if vim.bo.filetype == "help" then
      vim.bo[event.buf].buflisted = true
      vim.bo[event.buf].modifiable = true
      vim.bo[event.buf].readonly = false

      vim.api.nvim_exec2("wincmd L", {})
    end
  end
})

vim.api.nvim_create_autocmd("BufWritePost", {
  callback = function()
    if vim.bo.filetype == "help" then
        local file_path = vim.fn.expand("%:p")
        -- Async (vim.system) doesn't play well with expand.
        vim.fn.system("chezmoi re-add " .. file_path)
      end
  end
})
