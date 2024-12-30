if package.loaded["lazy"] then
  -- https://github.com/folke/lazy.nvim/issues/1180
  return
end

---@type LazyConfig
local opts = {
  defaults = {
    lazy = true,
  },

  local_spec = false,

  lockfile =  vim.fs.joinpath (vim.fn.stdpath ("data"), "..", "chezmoi", "dot_config", "nvim") .. "/lazy-lock.json",

  git = {
    log = { "-4" },
    timeout = 60,
  },

  pkg = {
    enabled = false,
  },

  rocks = {
    enabled = true,
  },

  dev = {
    path = vim.fs.joinpath (os.getenv ("HOME") or os.getenv ("USERPROFILE"), "Projects"),
    patterns = {
      os.getenv ("USER") or os.getenv ("USERNAME"),
    },
  },

  install = {
    colorscheme = { "default" },
  },

  ui = {
    backdrop = 100,
    size = { width = 0.9, height = 0.9 },
  },

  custom_keys = {
    ["<localleader>l"] = nil,
    ["<localleader>i"] = nil,
    ["<localleader>t"] = nil,
  },

  change_detection = {
    notify = false,
  },

  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "man",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "osc52",
        "rplugin",
        "spellfile",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },

  readme = {
    enabled = false,
  },
}

local id = vim.api.nvim_create_autocmd ("User", {
  once = true,
  pattern = "LazyInstall",
  callback = function ()
    if vim.o.filetype == "lazy" then
      vim.cmd.close ()
    end
  end,
})

vim.api.nvim_create_autocmd ("User", {
  once = true,
  pattern = "VeryLazy",
  callback = function ()
    if vim.o.filetype ~= "lazy" then
      pcall (vim.api.nvim_del_autocmd, id)
    end
  end,
})

require ("lazy").setup ("spec", opts)
