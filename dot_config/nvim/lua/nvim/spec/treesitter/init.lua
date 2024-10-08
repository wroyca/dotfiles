---@module "nvim-treesitter"

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter", main = "nvim-treesitter.configs", event = "VeryLazy",

  ---@type TSConfig
  opts = setmetatable ({
    ensure_installed = "all",
    ---@type { [string]: TSModule }
    modules = {
      highlight = {
        enable = true,
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          node_incremental = "v",
          node_decremental = "V",
        }
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
      if key ~= "modules" then
        local modules = rawget (table, "modules")
        if modules then
          for k, v in pairs (modules) do
            rawset (table, k, v)
          end
          rawset (table, "modules", nil)
          return rawget (table, key)
        end
      end
    end,
  }),
}

return Spec
