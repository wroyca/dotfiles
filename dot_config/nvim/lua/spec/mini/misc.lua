---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", virtual = true, event = "VeryLazy",

  config = function (_, opts)
    require ("mini.misc").setup (opts)

    -- We need to normalize the process state to the project root. Instead of
    -- trying to guess based on build files (which might be nested), we assume
    -- the repository root is the canonical anchor. This simplifies relative
    -- path logic elsewhere. Note that we use the global CWD change here;
    -- we assume a single-project session model.
    --
    MiniMisc.setup_auto_root ({ ".git" })

    -- Attempt to reconstruct the editing context by jumping to the last known
    -- position. We are relying on the default logic here, which hooks into
    -- BufReadPost. If we find this interfering with git commit messages or
    -- specific scratch buffers, we will need to come back and restrict the
    -- event filter.
    --
    MiniMisc.setup_restore_cursor ()

    -- We also need to harmonize the visual boundary between the TUI and the
    -- hosting terminal emulator. Since we cannot query the terminal for its
    -- theme reliably, we push our background color outwards via standard OSC
    -- escape sequences.
    --
    MiniMisc.setup_termbg_sync()
  end
}

return Spec
