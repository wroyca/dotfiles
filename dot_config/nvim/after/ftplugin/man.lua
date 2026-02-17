if vim.bo.filetype ~= "man" then
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
    if vim.bo.filetype == "man" then
      -- Move the current window to the far right.
      --
      vim.api.nvim_exec2 ("wincmd L", {})

      -- Unlike help buffers, man pages can accumulate and clutter the
      -- buffer list. We treat them as ephemeral references, so unlist
      -- the buffer to keep the environment clean.
      --
      vim.bo[ev.buf].buflisted = false
    end
  end,
})
