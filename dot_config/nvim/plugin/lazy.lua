if package.loaded["lazy"] then
  -- Keep plugin management separate from configuration logic. Note that
  -- lazy.nvim overrides Neovim's built-in loader, causing pack/*/start/* to be
  -- sourced again. The overhead is minimal since we mark the file as no-op
  -- after lazy.nvim loads (using package.loaded).
  --
  -- https://github.com/folke/lazy.nvim/issues/1180.
  --
  return
end

require ("lazy").setup ("nvim", {
  performance = {
    rtp = {
      disabled_plugins = {
        "2html_plugin",
        "bugreport",
        "ftplugin",
        "getscriptPlugin",
        "getscript",
        "gzip",
        "health",
        "logipat",
        "matchit",
        "matchparen",
        "netrwFileHandlers",
        "netrwPlugin",
        "netrwSettings",
        "netrw",
        "nvim",
        "optwin",
        "rplugin",
        "rrhelper",
        "spellfile",
        "spellfile_plugin",
        "synmenu",
        "syntax",
        "tarPlugin",
        "tar",
        "tohtml",
        "tutor",
        "vimballPlugin",
        "vimball",
        "zipPlugin",
        "zip",
      },
    },
  },

  defaults = {
    lazy = true,
    version = false,
  },

  pkg = {
    enabled = false,
  },

  readme = {
    enabled = false,
  },

  change_detection = {
    enabled = false,
  },

  install = {
    colorscheme = { "default" },
  },

  ui = {
    pills = false,
    border = "single",
    backdrop = 100,
  },

  dev = {
    path = "~/Projects",
    patterns = {
      "wroyca",
    },
  },
})

if vim.o.filetype == "lazy" then vim.cmd.close () end
