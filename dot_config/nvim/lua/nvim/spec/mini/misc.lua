---@module "mini.misc"

---@type LazyPluginSpec
local Spec = {
  "mini.misc", virtual = true, event = "VeryLazy",

  -- opts shouldn't call setup, as mini modules self-export through _G.
  config = function(_, opts)
    require("mini.misc").setup(opts)

    -- Finds root directory for current buffer file and sets
    -- |current-directory| to it (using |chdir()|).
    MiniMisc.setup_auto_root({ ".git" })

    -- When reopening a file this will make sure the cursor is placed back to
    -- the position where we left before.
    MiniMisc.setup_restore_cursor()
  end,
}

return Spec
