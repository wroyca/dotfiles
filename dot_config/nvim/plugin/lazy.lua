if package.loaded['lazy'] then return end

-- https://github.com/folke/lazy.nvim/issues/1180
--

require [[lazy]].setup ([[spec]], {
   performance = {
    rtp = {
      disabled_plugins = {
        [[2html_plugin]],
        [[bugreport]],
        [[ftplugin]],
        [[getscriptPlugin]],
        [[getscript]],
        [[gzip]],
        [[health]],
        [[logipat]],
        [[matchit]],
        [[matchparen]],
        [[netrwFileHandlers]],
        [[netrwPlugin]],
        [[netrwSettings]],
        [[netrw]],
        [[nvim]],
        [[optwin]],
        [[rplugin]],
        [[rrhelper]],
        [[spellfile]],
        [[spellfile_plugin]],
        [[synmenu]],
        [[syntax]],
        [[tarPlugin]],
        [[tar]],
        [[tohtml]],
        [[tutor]],
        [[vimballPlugin]],
        [[vimball]],
        [[zipPlugin]],
        [[zip]]
      }
    }
  },

  defaults = {
    lazy = true,
    version = false
  },

  pkg = {
    enabled = false
  },

  readme = {
    enabled = false
  },

  change_detection = {
    enabled = false
  },

  ui = {
    pills = false,
    backdrop = 100
  },

  dev = {
    path = [[~/.config]],
    patterns = {
      [[wroyca]]
    }
  }
})
