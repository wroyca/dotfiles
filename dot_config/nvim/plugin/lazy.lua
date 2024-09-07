if package.loaded["lazy"] then
  -- Keep plugin management separate from configuration logic. Note that
  -- lazy.nvim overrides Neovim's built-in loader, causing `pack/*/start/*` to be
  -- sourced again. The overhead is minimal since the files are marked as no-op
  -- after lazy.nvim loads (using `package.loaded`).
  --
  -- For more details, see: https://github.com/folke/lazy.nvim/issues/1180
  --
  return
end

-- Global system-wide dark mode preference.
--
-- Supported platforms are Linux, MacOS and Windows. All platform
-- implementations are interrupt-based and do not use any resources in the
-- background.
--
vim.cmd.packadd "vim-lumen"

-- Neovim 0.10.1 includes logic to test whether a given terminal emulator
-- supports truecolor and sets the default value of `termguicolors` to true.
-- The upstream Neovim project performs the following checks:
--
-- - If `COLORTERM=truecolor`, enable truecolor (`tgc`).
--
-- - If the `TERM` variable's terminfo reports support for `Tc` or `RGB`, enable
--   `tgc`.
--
-- - Query the terminal capability directly via OSC 11, enabling `tgc` if the
--   color rendered matches the configured color.
--
-- Unfortunately, the latter method can cause a "flash" visual effect in some
-- terminals (e.g., macOS's Terminal.app / nsterm) as truecolor is enabled and
-- then disabled again. It is also exacerbated by blocking operations (e.g.,
-- plugin manager setup). This behavior is suboptimal.
--
-- For more details, see: https://github.com/neovim/neovim/issues/29966
--
if ((vim.env.TERM:match("-direct")) or (vim.env.TERM:match("-256color"))) and not (vim.env.TERM == "nsterm-256color") then
  vim.o.termguicolors = true
end

--- Set up terminal background synchronization
---
--- What it does:
---
--- - Checks if the terminal emulator supports the OSC 11 control sequence through
---   the appropriate `stdout`. Stops if not supported.
---
--- - Creates autocommands for |ColorScheme| and |VimResume| events to change
---   the terminal background to match the |guibg| color of |hl-Normal|.
---
--- - Creates autocommands for |VimLeavePre| and |VimSuspend| events to reset
---   the terminal background.
---
--- - Synchronizes the background immediately to avoid dependency on loading order.
---
--- The primary use case is to eliminate any "frame" around the current Neovim instance
--- that appears if Neovim's |hl-Normal| background color differs from what is
--- used by the terminal emulator itself.
---
local setup_termbg_sync = function()
  local has_stdout_tty = false
  for _, ui in ipairs(vim.api.nvim_list_uis()) do
    has_stdout_tty = has_stdout_tty or ui.stdout_tty
  end
  if not has_stdout_tty then return end

  local f = function()
    local sync = function()
      local normal = vim.api.nvim_get_hl(0, { name = "Normal" })
      if normal.bg == nil then return end
      io.stdout:write(string.format("\027]11;#%06x\007", normal.bg))
    end
    vim.api.nvim_create_autocmd({ "VimResume", "ColorScheme" }, { callback = sync })
    local reset = function() io.stdout:write("\027]111;;\007") end
    vim.api.nvim_create_autocmd({ "VimLeavePre", "VimSuspend" }, { callback = reset })
    sync()
  end

  local id = vim.api.nvim_create_autocmd("TermResponse", { callback = f, once = true, nested = true })
  io.stdout:write("\027]11;?\007")
  vim.defer_fn(function()
    local ok = pcall(vim.api.nvim_del_autocmd, id)
    if ok then H.notify("`setup_termbg_sync()` did not get response from terminal emulator", "WARN") end
  end, 1000)
end
setup_termbg_sync()

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
      "default",
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

local id = vim.api.nvim_create_autocmd("User", {
  once = true,
  pattern = "LazyInstall",
  callback = function()
    -- LazyInstall is used here instead of VeryLazy because VeryLazy causes an
    -- error related to an invalid buffer ID.
    --
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
    end
  end
})

vim.api.nvim_create_autocmd("User", {
  once = true,
  pattern = "VeryLazy",
  callback = function()
    if vim.o.filetype ~= "lazy" then
      -- Delete LazyInstall autocmd at this point to prevent it from closing
      -- LazyFloat during any subsequent manual synchronization operations.
      --
      pcall(vim.api.nvim_del_autocmd, id)
    end
  end
})

require ("lazy").setup("nvim", opts)
