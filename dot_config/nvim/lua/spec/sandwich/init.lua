---@module "vim-sandwich"

---@type LazyPluginSpec
local Spec = {
  "machakann/vim-sandwich", event = "VeryLazy",

  -- Use vim.cmd in this context to prevent any potential key remapping
  -- mistakes.
  config = function ()
    vim.cmd ([[
      runtime macros/sandwich/keymap/surround.vim

      nunmap sa
      xunmap sa
      ounmap sa
      nunmap sr
      xunmap sr
      nunmap sd
      xunmap sd
      nunmap srb
      nunmap sdb

      nmap gsa  <Plug>(sandwich-add)
      xmap gsa  <Plug>(sandwich-add)
      omap gsa  <Plug>(sandwich-add)
      nmap gsr  <Plug>(sandwich-replace)
      xmap gsr  <Plug>(sandwich-replace)
      nmap gsrb <Plug>(sandwich-replace-auto)
      nmap gsd  <Plug>(sandwich-delete)
      xmap gsd  <Plug>(sandwich-delete)
      nmap gsdb <Plug>(sandwich-delete-auto)
    ]])
  end,
}

return Spec
