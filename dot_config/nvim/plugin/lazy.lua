if package.loaded["lazy"] then
  -- https://github.com/folke/lazy.nvim/issues/1180
  return
end

local lazypath = vim.fs.joinpath (vim.fn.stdpath ("data") --[[ @as string ]], "lazy", "lazy.nvim")
if not vim.uv.fs_stat (lazypath) then
  vim
    .system ({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim",
      lazypath,
    })
    :wait ()
end
vim.opt.rtp:prepend (lazypath)

---@type LazyConfig
local opts = {
  defaults = {
    lazy = true,
  },

  local_spec = false,

  git = {
    log = { "-4" },
    timeout = 60,
  },

  pkg = {
    enabled = false,
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
    border = "single",
    backdrop = 100,
    pills = false,
  },

  custom_keys = {
    ["<localleader>l"] = nil,
    ["<localleader>t"] = nil,
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
