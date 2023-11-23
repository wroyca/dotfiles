---@type LazyPluginSpec
return {
  {
    [[L3MON4D3/LuaSnip]],
    event = [[InsertCharPre]],
    dependencies = {
      {
        [[rafamadriz/friendly-snippets]],
        config = function()
          vim.schedule(function()
            require [[luasnip.loaders.from_vscode]].lazy_load()
          end)
        end
      }
    }
  },
  {
    [[saadparwaiz1/cmp_luasnip]],
    event = [[InsertCharPre]],
  }
}
