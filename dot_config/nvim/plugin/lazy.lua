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

-- https://github.com/neovim/neovim/issues/29966
--
vim.o.termguicolors = true

---@type LazyConfig
local opts = {
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
    colorscheme = {
      "mini",
      "default"
    },
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
}

vim.api.nvim_create_autocmd("User", {
  once = true,
  pattern = "LazyInstall",
  callback = function()
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
    end
  end
})

require ("lazy").setup("nvim", opts)
