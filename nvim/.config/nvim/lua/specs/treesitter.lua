return {
 "nvim-treesitter/nvim-treesitter",
  version = false, -- last release is way too old and doesn't work on Windows
  build = function()
    pcall(require("nvim-treesitter.install").update({ with_sync = true }))
  end,
  event = { "BufReadPost", "BufNewFile" },
  keys = {
    { "<c-space>", desc = "Increment selection" },
    { "<bs>", desc = "Schrink selection", mode = "x" },
  },
  opts = {
    highlight = { enable = true },
    indent = { enable = true, disable = { "python" } },
    context_commentstring = { enable = true, enable_autocmd = false },
    ensure_installed = {
      "bash",
      "c",
      "cpp",
      "help",
      "json",
      "lua",
      "markdown",
      "markdown_inline",
      "python",
      "query",
      "regex",
      "vim",
      "yaml"
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<C-space>",
        node_incremental = "<C-space>",
        scope_incremental = "<nop>",
        node_decremental = "<bs>",
      },
    },
  },
  config = function(_, opts)
    require("nvim-treesitter.configs").setup(opts)
  end
}
