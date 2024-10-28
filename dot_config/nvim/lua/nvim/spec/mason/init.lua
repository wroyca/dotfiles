---@type LazyPluginSpec
local Spec = {
  "williamboman/mason.nvim", dependencies = {{ "mini.icons", dev = true }},

  opts = function()
    local icons = require ("mini.icons")
    return {
      max_concurrent_installers = 10,
      pip = {
        upgrade_pip = true,
      },
      ui = {
        icons = {
          package_installed = icons.get("file", "done_sym"),
          package_pending = icons.get("file", "working_sym"),
          package_uninstalled = icons.get("file", "removed_sym"),
        }
      }
    }
  end
}

return Spec
