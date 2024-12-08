---@module "mini.notify"

---@type LazyPluginSpec
local Spec = {
  "mini.notify", virtual = true, event = "VeryLazy",

  -- opts shouldn't call setup, as mini modules self-export through _G.
  config = function(_, opts)
    require("mini.notify").setup(opts)

    -- Creates an implementation of |vim.notify()| powered by this module.
    -- General idea is that notification is shown right away (as soon as safely
    -- possible, see |vim.schedule()|) and removed after a configurable amount
    -- of time.
    vim.notify = MiniNotify.make_notify(opts)
  end,
}

return Spec
