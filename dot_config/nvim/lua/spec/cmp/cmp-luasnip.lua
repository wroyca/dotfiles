--@type LazyPluginSpec
return {
  [[saadparwaiz1/cmp_luasnip]],
  name = [[cmp-luasnip]],

  dependencies = {
    {
      [[L3MON4D3/LuaSnip]],
      name = [[cmp-luasnip-loaders]],
      config = function()
        require [[luasnip.loaders.from_vscode]].lazy_load()
      end
    }
  },

  opts = function()
    local types = require [[luasnip.util.types]]
    return {
      -- Show cursor-style placeholder in unvisited nodes.
      ext_opts = {
        [types.insertNode] = {
          unvisited = {
            virt_text = {
              {
                [[|]], [[Conceal]]
              }
            },
            virt_text_pos = [[inline]],
          },
        },
        [types.exitNode] = {
          unvisited = {
            virt_text = {
              {
                [[|]], [[Conceal]]
              }
            },
            virt_text_pos = [[inline]]
          }
        }
      }
    }
  end
}
