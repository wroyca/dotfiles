local create_autocmd = vim.api.nvim_create_autocmd
local create_augroup = vim.api.nvim_create_augroup

create_autocmd([[VimResized]], {
  desc = [[Maintain consistent window dimensions while adjusting nvim size]],
  callback = function()
    -- Side effect: The last tab page (or where an error occurred) becomes the
    -- current tab page. This is undesired in our context, so save and restore
    -- our current tab page prior and after tabdo operation.
    --
    -- https://neovim.io/doc/user/tabpage.html#%3Atabdo
    --
    local current_tab_page = vim.fn.tabpagenr()
    vim.api.nvim_exec2([[tabdo wincmd =]], {})
    vim.api.nvim_exec2([[tabnext ]] .. current_tab_page, {})
  end
})

vim.api.nvim_create_autocmd("CmdWinEnter", {
  desc = [[Close the command-line window with q]],
  pattern = "[:/?=]",
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set([[n]], [[q]], [[<Cmd>q<CR>]], { buffer = event.buf, silent = true })
  end
})

vim.api.nvim_create_autocmd([[ColorScheme]], {
  desc = [[Mini highlights]],
  callback = function(event)
    -- Script variant with Comment
    --
    vim.cmd [[highlight Comment  gui=italic]]

    -- https://github.com/echasnovski/mini.nvim/issues/658
    --
    vim.cmd [[highlight LeapMatch  gui=underline]]

    -- https://github.com/echasnovski/mini.nvim/issues/538
    --
    vim.cmd [[highlight! link DiffAdd    DiagnosticOk]]
    vim.cmd [[highlight! link DiffChange DiagnosticWarn]]
    vim.cmd [[highlight! link DiffDelete DiagnosticError]]
  end
})

