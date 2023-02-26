return {
 "nvim-treesitter/nvim-treesitter",
  version = false, -- last release is way too old and doesn't work on Windows
  build = function()
    pcall(require("nvim-treesitter.install").update({ with_sync = true }))
  end,
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    highlight = { enable = true },
    context_commentstring = { enable = true, enable_autocmd = false },
    ensure_installed = {
      "c", "cpp",
    },
  },
  config = function(_, opts)
    require("nvim-treesitter.configs").setup(opts)
  end
}
