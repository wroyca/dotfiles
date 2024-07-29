---@diagnostic disable: missing-fields
---@type LazyPluginSpec
return {
  [[nvim-treesitter/nvim-treesitter]], main = [[nvim-treesitter.configs]], event = [[VeryLazy]],

  ---@type TSConfig
  opts = setmetatable({
    ensure_installed = [[all]],
    ---@type {[string]:TSModule}
    modules = {
      highlight = {
        enable = true
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          node_incremental = [[v]],
          node_decremental = [[V]]
        }
      }
    }
  },

  -- HACK:
  --
  -- Use a metatable to unpack the modules table at runtime, automatically
  -- inlining its fields into opts.
  {
    -- TSConfig expects a modules field, but Treesitter's internal logic also
    -- dynamically creates it. This situation results in our modules field nesting
    -- within Treesitter's dynamically created one. Generally, various Neovim
    -- configurations approach this by manually inlining the configuration (e.g.):
    --
    -- opts = {
    --   ---@type TSModule
    --   highlight = {
    --     enable = false
    --   }
    -- }
    --
    -- Although this approach is technically valid, the class annotation displays
    -- `@field modules {[string]:TSModule}`, implying that we should configure
    -- modules through the `modules` table. Being unaware of this internal quirk
    -- and following this suggestion could lead to a silent merge of what seems
    -- like a valid Lua table, resulting in a broken configuration that might go
    -- unnoticed, particularly if other highlighting methods are layered.
    __index = function(table, key)
      if key ~= [[modules]] then
        local modules = rawget(table, [[modules]])
        if modules then
          for k, v in pairs(modules) do
            rawset(table, k, v)
          end
          rawset(table, [[modules]], nil)
          return rawget(table, key)
        end
      end
    end,
    __newindex = function(table, key, value)
      if key ~= [[modules]] then
        rawset(table, key, value)
      end
    end
  })
}
