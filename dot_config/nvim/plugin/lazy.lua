if package.loaded["lazy"] then
  -- Keep plugin management separate from configuration logic.
  --
  -- Note that lazy.nvim overrides Neovim's built-in loader, causing plugin/*/
  -- to be sourced again. The overhead is minimal since the files are marked as
  -- no-op after lazy.nvim loads (using package.loaded).
  --
  -- For more details, see: https://github.com/folke/lazy.nvim/issues/1180
  --
  return
end

assert(jit.status(), "JIT is inadvertently switched off.")

-- Neovim 0.10.1 includes logic to test whether a given terminal emulator
-- supports truecolor and sets the default value of `termguicolors` to true.
--
-- Unfortunately, this process occurs only after Neovim has finished handling
-- ongoing tasks, and the situation is further exacerbated by blocking
-- operations, such as plugin manager setup, making this behavior suboptimal.
--
-- For more details, see: https://github.com/neovim/neovim/issues/29966
--
vim.o.termguicolors = true

-- Determine the system's preferred color scheme. Supported platforms are
-- Linux, MacOS and Windows. All platform implementations are interrupt-based
-- and do not use any resources in the background.
--
-- Note: Neovim typically infers this information from the terminal's
-- lightness. Unfortunately, this check presents the same issues as with
-- termguicolors (see above for details).
--
vim.cmd.packadd "lumen"

-- Terminal Color Synchronization with OSC 10. This assumes that the terminal
-- supports the OSC 111 extension as well.
--
local has_stdout_tty = false
for _, ui in ipairs(vim.api.nvim_list_uis()) do
  has_stdout_tty = has_stdout_tty or ui.stdout_tty
end
if has_stdout_tty then
  vim.api.nvim_create_autocmd({ "VimEnter", "VimResume", "ColorScheme" }, {
    callback = function()
      io.stdout:write(string.format("\027]11;#%06x\007", vim.api.nvim_get_hl(0, { name = "Normal" }).bg))
    end,
  })
  vim.api.nvim_create_autocmd({ "VimLeavePre", "VimSuspend" }, {
    callback = function()
      io.stdout:write("\027]111;;\007")
    end,
  })
end

---@type LazyConfig
local opts = {
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "netrwPlugin",
        "rplugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
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
      "default",
    },
  },

  ui = {
    pills = false,
    border = "single",
    backdrop = 100,
  },

  dev = {
    path = vim.fs.joinpath(os.getenv "HOME" or os.getenv "USERPROFILE", "Projects"),
    patterns = {
      os.getenv "USER" or os.getenv "USERNAME",
    },
  },
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
