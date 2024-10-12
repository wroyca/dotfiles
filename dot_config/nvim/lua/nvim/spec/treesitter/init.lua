---@module "nvim-treesitter"
---@diagnostic disable: missing-fields

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
        disable = function(_, bufnr)
          -- For large files, parsing may slow down, causing user input to
          -- block. To mitigate this, disable TSHighlight when file size
          -- exceeds 1 MB.
          --
          -- For more details, see
          -- https://github.com/neovim/neovim/issues/22426
          --
          return (vim.fn.getfsize(bufnr) > 1000000) or false
        end
      },
    },
  },

  -- The TSConfig annotation shows `@field modules { [string]: TSModule }`, but
  -- Tree-sitter's internal logic dynamically creates the modules field at
  -- runtime, causing opts.modules to be nested within it.
  --
  -- To address this, the current strategy is to unpack the fields from
  -- opts.modules directly into opts and then dynamically remove opts.modules
  -- at runtime. This approach should enable Tree-sitter's internal logic to
  -- correctly parse the modules configuration, however they do so.
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
