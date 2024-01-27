--@type LazyPluginSpec
return {
  [[cmp-luasnip]],
  event = [[InsertCharPre]],
  dependencies = {
    {
      [[cmp-luasnip-loaders]],
      config = function()
        require [[luasnip.loaders.from_vscode]].lazy_load()
      end
    }
  }
}
