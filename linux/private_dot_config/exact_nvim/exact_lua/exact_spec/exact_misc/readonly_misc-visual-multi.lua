---@type LazyPluginSpec
return {
  [[misc-visual-multi]],
  keys = { [[<C-Down>]], [[<C-Up>]] },
  config = function()
    vim.g.VM_silent_exit = 1
    vim.g.VM_show_warnings = 0
    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[visual_multi_exit]],
      callback = function()
        vim.o.cmdheight = 1
        vim.schedule(function()
          vim.o.cmdheight = 0
        end)
      end
    })
  end
}
