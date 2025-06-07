---@module "mason"

---@type LazyPluginSpec
local Spec = {
  "mason-org/mason.nvim",

  cmd = {
    "Mason",
    "MasonInstall",
    "MasonInstallAll",
    "MasonUninstall",
    "MasonUninstallAll",
    "MasonLog"
  },

  opts = {
    pip = {
      upgrade_pip = true
    },
    ui = {
      backdrop = 100,
    }
  }
}

return Spec
