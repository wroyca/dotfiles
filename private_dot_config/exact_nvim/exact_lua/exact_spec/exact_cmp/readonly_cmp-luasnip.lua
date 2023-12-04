--@type LazyPluginSpec
return {
  [[saadparwaiz1/cmp_luasnip]],
  name = [[cmp-luasnip]],
  event = [[InsertCharPre]],
  dependencies = {
    {
      [[L3MON4D3/LuaSnip]],
      name = [[cmp-luasnip-core]]
    },
    {
      [[rafamadriz/friendly-snippets]],
      name = [[cmp-luasnip-snippets]],
      config = function()
        require [[luasnip]].filetype_extend([[cpp]], { [[license]] })
        require [[luasnip.loaders.from_vscode]].lazy_load()
      end
    }
  }
}
