--@type LazyPluginSpec
return {
  [[saadparwaiz1/cmp_luasnip]],
  name = [[cmp-luasnip]],
  event = [[InsertCharPre]],
  dependencies = {
    {
      [[L3MON4D3/LuaSnip]],
      name = [[cmp-luasnip-engine]],
      config = function()
        require [[luasnip.loaders.from_vscode]].lazy_load()
      end
    }
  }
}
