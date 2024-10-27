if package.loaded["lazy"] then
  -- https://github.com/folke/lazy.nvim/issues/1180
  --
  return
end

assert(jit.status(), "error: jit compiler is unavailable")

vim.g.loaded_gzip = 1
vim.g.loaded_man = true
vim.g.loaded_matchit = 1
vim.g.loaded_matchparen = 1
vim.g.loaded_netrwPlugin = "v173"
vim.g.loaded_remote_plugins = 1
vim.g.loaded_spellfile_plugin = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_tutor_mode_plugin = 1
vim.g.loaded_zipPlugin = 1

---@type LazyConfig
local opts = {
  defaults = {
    lazy = true,
  },

  local_spec = false,

  git = {
    log = { "-4" },
    timeout = 60
  },

  pkg = {
    enabled = false
  },

  dev = {
    path = vim.fs.joinpath(os.getenv "HOME" or os.getenv "USERPROFILE", "Projects"),
    patterns = {
      os.getenv "USER" or os.getenv "USERNAME",
    },
  },

  install = {
    colorscheme = { "default" },
  },

  ui = {
    border = "single",
    backdrop = 100,
    pills = false,
  },

  custom_keys = {
    ["<localleader>l"] = nil,
    ["<localleader>t"] = nil
  },

  diff = {
    cmd = "diffview.nvim",
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
    }
  },

  readme = {
    enabled = false,
  }
}

local id = vim.api.nvim_create_autocmd("User", {
  once = true,
  pattern = "LazyInstall",
  callback = function()
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
    end
  end,
})

vim.api.nvim_create_autocmd("User", {
  once = true,
  pattern = "VeryLazy",
  callback = function()
    if vim.o.filetype ~= "lazy" then
      pcall(vim.api.nvim_del_autocmd, id)
    end
  end,
})

require("lazy").setup("nvim", opts)
