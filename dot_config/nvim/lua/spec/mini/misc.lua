---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", virtual = true, lazy = false,

  -- opts shouldn't call setup, as this module self-export through _G.
  config = function (_, opts)
    require ("mini.misc").setup (opts)

    -- Automatically sets the working directory to the root of the current
    -- buffer using common project markers (e.g. `.git`).
    MiniMisc.setup_auto_root ({ ".git" })

    -- Restores the cursor to its last known position when reopening a file.
    MiniMisc.setup_restore_cursor ()

    -- Synchronizes the terminal emulator's background color with the editor's
    -- active color scheme by using standard terminal escape sequences (OSC
    -- codes).
    --
    -- Background: The original version of this function chose not to use OSC
    -- 111 (which resets the terminal's background color to its default) out of
    -- caution. That sequence is considered an extension and, in theory, not all
    -- terminal emulators may support it. To avoid relying on it, the function
    -- instead queried the terminal's current background color (via OSC 11) and
    -- cached it with the intention of restoring it later.
    --
    -- In practice, however, this strategy tends to cause more problems than it
    -- solves. Caching a background color at startup assumes that the terminal's
    -- appearance will remain fixed across the editor's lifetime, but this isn't
    -- always the case. For instance, users may switch from a dark terminal
    -- theme to a light one mid-session. In that scenario, restoring a
    -- previously cached dark background into a now-light environment can
    -- produce mismatched or unreadable text (e.g., dark-on-dark).
    --
    -- Given how widespread OSC 111 support is in modern terminals, it is both
    -- simpler and more robust to use it directly. This allows the terminal to
    -- restore its own default background color on exit, typically the same
    -- value it would have used at launch, without relying on cached state or
    -- manual guesswork.
    --
    -- As such, we now adopt the intended approach: use OSC 11 to align the
    -- terminal background with Neovim's `Normal` highlight group, and use OSC
    -- 111 on suspend or exit to return control to the terminal.
    MiniMisc.setup_termbg_sync = (function ()
      -- Terminal background synchronization only makes sense in contexts where
      -- there is a usable stdout-backed terminal. For example, when Neovim is
      -- embedded in a headless session or fronted by a GUI, issuing OSC
      -- sequences would be irrelevant or outright ignored. We check for at
      -- least one UI with a valid stdout TTY before proceeding.
      local has_stdout_tty = false
      for _, ui in ipairs (vim.api.nvim_list_uis ()) do
        has_stdout_tty = has_stdout_tty or ui.stdout_tty
      end
      if not has_stdout_tty then
        return
      end

      -- Autocommands are grouped under a dedicated namespace to avoid layering
      -- duplicate handlers if the function is re-run.
      local augroup = vim.api.nvim_create_augroup ("MiniMiscTermbgSync", { clear = true })

      -- Push the current background color from the 'Normal' highlight group to
      -- the terminal. This brings the terminal's ambient background into
      -- alignment with Neovim's UI, which helps avoid visual dissonance at
      -- theme boundaries where the terminal may otherwise retain stale visuals.
      local sync = function ()
        local normal = vim.api.nvim_get_hl (0, { name = "Normal" })
        if normal.bg then
          -- Emit OSC 11 with the background encoded as a 6-digit RGB hex
          -- string. Most modern terminals interpret this as a request to change
          -- the default background color for the entire display.
          io.stdout:write (string.format ("\027]11;#%06x\007", normal.bg))
        end
      end

      -- Hand terminal background control back to the terminal itself.
      local reset = function ()
        -- OSC 111 requests the terminal to restore its default background color
        -- to avoid leaving behind any temporary color overrides we introduced
        -- via OSC 11.
        io.stdout:write ("\027]111\007")
      end

      -- We reapply the background color after theme changes and on resume.
      --
      -- This covers both user-triggered color scheme switches and session
      -- resumes (e.g., after a shell suspend). In either case, reasserting the
      -- intended background avoids drift between the editor's palette and the
      -- terminal's.
      vim.api.nvim_create_autocmd ({ "VimResume", "ColorScheme" }, {
        group = augroup,
        callback = sync,
      })

      -- When suspending or exiting, we cleanly return control of the terminal's
      -- background appearance to avoid leaking visual assumptions into the
      -- terminal session the user returns to.
      vim.api.nvim_create_autocmd ({ "VimLeavePre", "VimSuspend" }, {
        group = augroup,
        callback = reset,
      })

      -- Apply the background immediately at startup to force the terminal
      -- appearance to be synchronized from the outset, rather than lagging
      -- behind until the next event hook.
      sync ()
    end) ()
  end,
}

return Spec
