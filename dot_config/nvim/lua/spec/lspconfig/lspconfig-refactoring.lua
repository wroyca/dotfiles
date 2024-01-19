---@type LazyPluginSpec
return {
  [[lspconfig-refactoring]],
  config = true,
  keys = {
    { [[<leader>lef]],  [[:Refactor extract ]],              mode = [[x]],            desc = [[Extract function]] },
    { [[<leader>leF]],  [[:Refactor extract_to_file ]],      mode = [[x]],            desc = [[Extract function to file]] },
    { [[<leader>lev]],  [[:Refactor extract_var ]],          mode = [[x]],            desc = [[Extract variable]] },
    { [[<leader>liv]],  [[:Refactor inline_var]],            mode = { [[x]], [[n]] }, desc = [[Inline variable]] },
    { [[<leader>lif]],  [[:Refactor inline_func]],           mode = [[n]],            desc = [[Inline function]] },
    { [[<leader>leb]],  [[:Refactor extract_block]],         mode = [[n]],            desc = [[Extract block]] },
    { [[<leader>leB]],  [[:Refactor extract_block_to_file]], mode = [[n]],            desc = [[Extract block to file]] },
  }
}
