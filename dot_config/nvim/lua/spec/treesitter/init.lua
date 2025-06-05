---@module "nvim-treesitter"

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter", main = "nvim-treesitter.configs", build = ":TSUpdate", event = "VeryLazy",

  ---@type TSConfig
  opts = setmetatable ({
    ensure_installed = "all",
    parser_install_dir = vim.fs.joinpath (vim.fn.stdpath ("data"), "..", "chezmoi", "dot_config", "nvim"),
    ---@type { [string]: TSModule }
    modules = {
      highlight = {
        enable = true,
      },
    },
  }, {
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
