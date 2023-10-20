---@type LazySpec
return {
  [[saadparwaiz1/cmp_luasnip]],
  event = [[InsertCharPre]],
  dependencies = {
    {
      [[L3MON4D3/LuaSnip]]
    },
    {
      [[rafamadriz/friendly-snippets]],
      config = function()
        require [[luasnip.loaders.from_vscode]].lazy_load()
      end
    }
  }
}
