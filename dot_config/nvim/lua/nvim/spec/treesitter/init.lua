---@module "nvim-treesitter"

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter", main = "nvim-treesitter.configs", build = ":TSUpdate", event = "VeryLazy",

  ---@type TSConfig
  ---@diagnostic disable: missing-fields
  opts = setmetatable({
    ensure_installed = "all",
    ---@type { [string]: TSModule }
    modules = {
      highlight = {
        enable = true,
        disable = function(_, bufnr)
          -- https://github.com/neovim/neovim/issues/22426
          return (vim.fn.getfsize(bufnr) > 1000000) or false
        end,
      },
    },
  }, {
    -- TSConfig annotation specifies `@field modules { [string]: TSModule }`,
    -- but Tree-sitter generates the modules field dynamically at runtime. To
    -- address this, unpack opts.modules directly into opts, then remove the
    -- original field.
    __index = function(table, key)
      if key ~= "modules" then
        local modules = rawget(table, "modules")
        if modules then
          for k, v in pairs(modules) do rawset(table, k, v) end
          rawset(table, "modules", nil)
          return rawget(table, key)
        end
      end
    end,
  }),
}

return Spec
