---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", virtual = true, lazy = false,

  -- opts shouldn't call setup, as this module self-export through _G.
  config = function (_, opts)
    require ("mini.misc").setup (opts)

    -- Automatically sets the working directory to the root of the current buffer
    -- using common project markers (e.g. `.git`).
    MiniMisc.setup_auto_root ({ ".git" })

    -- Restores the cursor to its last known position when reopening a file.
    MiniMisc.setup_restore_cursor ()
      
    -- Synchronizes the terminal emulator's background color with the editor's
    -- current background (`Normal` highlight group's `guibg`).
    MiniMisc.setup_termbg_sync ()
  end,
}

return Spec
