---@module "nvim-treesitter"
---@diagnostic disable: missing-fields

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter", main = "nvim-treesitter.configs", build = ":TSUpdate", event = "VeryLazy",

  ---@type TSConfig
  opts = setmetatable ({
    ensure_installed = "all",
    -- BUG: These can only generate parsers with ABI version 13 - 14, not 15.
    ignore_install = { "scfg", "ocamllex", "teal", "unison", "mlir", "swift", "latex" },
    -- NOTE: remember to update vim.opt.runtimepath if location is changed.
    parser_install_dir = vim.fs.joinpath (vim.fn.stdpath ("data"), "..", "chezmoi", "dot_config", "nvim"),
    ---@type { [string]: TSModule }
    modules = {
      highlight = {
        enable = true,
      },
    },
  }, {
    -- TSConfig annotation specifies `@field modules { [string]: TSModule }`,
    -- but Tree-sitter generates the modules field dynamically at runtime. To
    -- address this, unpack opts.modules directly into opts, then remove the
    -- original field.
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

vim.opt.runtimepath:append (Spec.opts.parser_install_dir)
return Spec
