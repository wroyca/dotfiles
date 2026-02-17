if vim.bo.filetype ~= "help" then
  return
end

-- Sanitize the help buffer presentation.
--
-- The default Neovim behavior is to open help files in a horizontal split
-- above the current window. This is a bit annoying on modern wide screens
-- where vertical space is at a premium and we have plenty of horizontal
-- room. So we want to move it to the far right, effectively treating it as
-- a sidebar reference.
--
-- To achieve this, we hook into the window system. Note that we cannot use
-- the `FileType` event here: it fires too early, often before the buffer is
-- actually displayed in a valid window. If we try to issue `wincmd` calls
-- at that stage, they might apply to the wrong window or be ignored entirely.
-- Instead, we use `BufWinEnter` which guarantees that the buffer is loaded
-- and associated with a window.
--
vim.api.nvim_create_autocmd ("BufWinEnter", {
  callback = function (ev)
    -- Double-check that we are dealing with a help buffer. While the
    -- outer check guards the script loading, `BufWinEnter` is global,
    -- so we need to filter again to be safe.
    --
    if vim.bo.filetype == "help" then
      -- Move the current window to the far right.
      --
      vim.api.nvim_exec2 ("wincmd L", {})

      -- While we are at it, map 'q' to close the window. This is convenient
      -- for read-only buffers.
      --
      vim.keymap.set ("n", "q", "<cmd>q<cr>", { buffer = ev.buf, silent = true })
    end
  end,
})
