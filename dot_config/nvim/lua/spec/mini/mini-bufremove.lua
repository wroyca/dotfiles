---@type LazyPluginSpec
return {
  [[mini.bufremove]],
  dev = true,

  keys = {
    {
      [[<leader>bd]],
      function()
        local function confirmation (bufnr)
          if vim.bo.modified then
            local choice = vim.fn.confirm(("Save changes to %q?"):format(vim.fn.bufname()), "&Yes\n&No\n&Cancel")
            require [[mini.bufremove]].delete(bufnr, choice == 2)
          else
            require [[mini.bufremove]].delete(bufnr)
          end
        end
        confirmation (0)
      end,
      desc = [[Delete]]
    },

    -- stylua: ignore
    {
      [[<leader>bD]],
      function()
        require [[mini.bufremove]].delete(0, true)
      end,
      desc = [[Delete (Force)]]
    }
  }
}
