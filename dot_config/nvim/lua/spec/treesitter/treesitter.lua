---@diagnostic disable: missing-fields
---@class nvim-treesitter.opts.modules.incremental_selection.keymaps
---@field init_selection string|boolean In normal mode, start incremental selection.
---@field node_incremental string|boolean In visual mode, increment to the upper named parent.
---@field scope_incremental string|boolean In visual mode, increment to the upper scope
---@field node_decremental string|boolean In visual mode, decrement to the previous named node.

---@type LazyPluginSpec
local Spec = {
  [[nvim-treesitter/nvim-treesitter]], main = [[nvim-treesitter.configs]], event = [[VeryLazy]],

  init = function ()
    vim.cmd.vmenu [[PopUp.Node\ Incremental <cmd>:lua require"nvim-treesitter.incremental_selection".node_incremental()<cr>]]
    vim.cmd.vmenu [[PopUp.Node\ Decremental <cmd>:lua require"nvim-treesitter.incremental_selection".node_decremental()<cr>]]
  end,

  ---@type TSConfig
  opts = setmetatable ({
    ---@type { [string]:TSModule }
    modules = {
      highlight = {
        enable = true,
      },

      incremental_selection = {
        enable = true,
        ---@type nvim-treesitter.opts.modules.incremental_selection.keymaps
        keymaps = {
          node_incremental = [[v]],
          node_decremental = [[V]],
        },
      },
    },
  },

  -- TSConfig annotation displays `@field modules { [string]: TSModule }`, but
  -- Treesitter's internal logic dynamically creates the modules field at
  -- runtime, which causes `opts.modules` to nest within it.
  --
  -- For now, the strategy is to unpack `opts.modules` fields into `opts` and
  -- then dynamically remove `opts.modules` at runtime. This should allow
  -- Treesitter's internal logic to properly parse (however they do so) the
  -- modules configuration.
  --
  {
    __index = function (table, key)
      if key ~= [[modules]] then
        local modules = rawget (table, [[modules]])
        if modules then
          for k, v in pairs (modules) do
            rawset (table, k, v)
          end
          rawset (table, [[modules]], nil)
          return rawget (table, key)
        end
      end
    end,
    __newindex = function (table, key, value)
      if key ~= [[modules]] then rawset (table, key, value) end
    end,
  })
}

return Spec
