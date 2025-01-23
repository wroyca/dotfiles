---@module "conform"

---@type LazyPluginSpec
local Spec = {
  "stevearc/conform.nvim", event = "VeryLazy",

  keys = {
    {
      "<leader>;",
      function ()
        require ("conform").format ({ async = true, lsp_format = "fallback" })
      end,
      mode = "",
      desc = "Format",
    },
  },

  opts = {
    formatters_by_ft = {
      cpp = { "clang-format" },
      lua = { "stylua" },
    },
  },
}

return Spec
