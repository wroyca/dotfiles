if package.loaded["lazy"] then
  -- Keep plugin management separate from configuration logic.
  --
  -- Note that lazy.nvim overrides Neovim's built-in loader, causing
  -- pack/*/start/* to be sourced again. The overhead is minimal since the
  -- files are marked as no-op after lazy.nvim loads (using package.loaded).
  --
  -- For more details, see: https://github.com/folke/lazy.nvim/issues/1180
  --
  return
end

local id = vim.api.nvim_create_augroup ("Lazy", { clear = false })

-- Neovim 0.10.1 includes logic to test whether a given terminal emulator
-- supports truecolor and sets the default value of `termguicolors` to true.
--
-- Unfortunately, the latter method can cause a "flash" visual effect in some
-- terminals (e.g., macOS's Terminal.app / nsterm) as truecolor is enabled and
-- then disabled again. It is also exacerbated by blocking operations (e.g.,
-- plugin manager setup). This behavior is suboptimal.
--
-- For more details, see: https://github.com/neovim/neovim/issues/29966
--
vim.o.termguicolors = true

-- Set up terminal background synchronization
--
-- The primary use case is to eliminate any "frame" around the current Neovim
-- instance that appears if Neovim's |hl-Normal| background color differs from
-- what is used by the terminal emulator itself.
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

-- Determine the system's preferred color scheme, whether dark or light.
--
-- The org.freedesktop.appearance.color-scheme key has been standardized in the
-- XDG Desktop Portal specification. Supported values are:
--
-- https://github.com/flatpak/xdg-desktop-portal/issues/629
--
local function parse_color_scheme(line)
  vim.o.background = (tonumber(line:match("uint32 (%d+)")) or 0) % 2 == 0 and "light" or "dark"
end
parse_color_scheme(
  vim.fn.system(
    "gdbus call -t 1 --session --dest=org.freedesktop.portal.Desktop --object-path=/org/freedesktop/portal/desktop --method=org.freedesktop.portal.Settings.Read org.freedesktop.appearance color-scheme"
  )
)
vim.fn.jobstart(
  "gdbus monitor --session --dest org.freedesktop.portal.Desktop --object-path /org/freedesktop/portal/desktop",
  {
    on_stdout = function(_, data)
      local line = table.concat(data)
      if string.find(line, "color%-scheme") then
        parse_color_scheme(line)
      end
    end,
  }
)
vim.api.nvim_create_autocmd({ "OptionSet" }, {
  pattern = "background",
  callback = function()
    vim.cmd.doautocmd("colorscheme")
  end,
})

-- Hide Neovim UI when LazyFloat is shown.
--
-- The idea is that if LazyFloat is displayed, Neovim is installing or
-- synchronizing plugin specs. It makes sense to treat this state as if Neovim
-- isn't ready for usage, and hide any UI elements until Lazy is done.
--
local ui = {
  state = {
    number = vim.o.number,
    ruler = vim.o.ruler,
    laststatus = vim.o.laststatus,
    cursorline = vim.o.cursorline,
  }
}

ui.state.lock = function()
  vim.o.number = false
  vim.o.ruler = false
  vim.o.laststatus = 0
  vim.o.cursorline = false

  vim.cmd.hi("cursor", "blend=100")
  vim.opt_local.guicursor:append { "a:cursor/lcursor" }
end

ui.state.unlock = function()
  vim.o.number = ui.state.number
  vim.o.ruler = ui.state.ruler
  vim.o.laststatus = ui.state.laststatus
  vim.o.cursorline = ui.state.cursorline

  vim.cmd.hi("cursor", "blend=0")
  vim.opt_local.guicursor:remove { "a:cursor/lcursor" }
end

vim.api.nvim_create_autocmd("ColorScheme", {
  once = true,
  group = "Lazy",
  callback = ui.state.lock
})

---@type LazyConfig
local opts = {
  root = vim.fs.joinpath(vim.fn.stdpath("config") --[[@as string]], "pack", "snapshot", "opt"),

  performance = {
    rtp = {
      disabled_plugins = {
        "2html_plugin",
        "bugreport",
        "ftplugin",
        "getscript",
        "getscriptPlugin",
        "gzip",
        "health",
        "logipat",
        "matchit",
        "matchparen",
        "netrw",
        "netrwFileHandlers",
        "netrwPlugin",
        "netrwSettings",
        "nvim",
        "optwin",
        "rplugin",
        "rrhelper",
        "spellfile",
        "spellfile_plugin",
        "synmenu",
        "syntax",
        "tar",
        "tarPlugin",
        "tohtml",
        "tutor",
        "vimball",
        "vimballPlugin",
        "zip",
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

vim.api.nvim_create_autocmd("User", {
  once = true,
  group = "Lazy",
  pattern = "LazyInstall",
  callback = function()
    if vim.o.filetype == "lazy" then
      vim.cmd.close()
    end
  end,
})

vim.api.nvim_create_autocmd("User", {
  once = true,
  group = "Lazy",
  pattern = "VeryLazy",
  callback = function()
    if vim.o.filetype ~= "lazy" then
      pcall(vim.api.nvim_del_augroup_by_id, id)
      ui.state.unlock() vim.cmd.redraw()
    end
  end,
})

require("lazy").setup("nvim", opts)
