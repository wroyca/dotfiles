---@type LazyPluginSpec
return {
  [[echasnovski/mini.tabline]],
  event = [[BufAdd]],

  keys = {
    {
      [[<C-Right>]],
      [[<cmd>bnext<cr>]],
      [[n]],
      desc = [[Tab Next]]
    },
    {
      [[<C-Left>]],
      [[<cmd>bprevious<cr>]],
      [[n]],
      desc = [[Tab Prev]]
    }
  },

  config = function()
    require('mini.tabline').setup({})
    vim.keymap.set('n', '<C-c>', function()
      for _, bufinfo in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
        if bufinfo.bufnr ~= vim.fn.bufnr() then
          vim.print(bufinfo.bufnr)
          vim.api.nvim_buf_delete(bufinfo.bufnr, { force = true })
        end
      end

      vim.cmd('redrawtabline')   -- Delete other buffers does not automatically redraw tabline, so do this manually.
    end, { desc = 'Close other buffers' })
  end,
}
