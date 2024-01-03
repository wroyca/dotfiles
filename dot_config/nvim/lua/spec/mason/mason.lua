---@diagnostic disable: undefined-field, unused-local
---@type LazyPluginSpec
return {
  [[williamboman/mason.nvim]],
  name = [[mason]],

  build = function()
    local m  = require [[mason]]
    local mr = require [[mason-registry]]
    local function ensure_installed()
      for _, tool in ipairs({
        [[clangd]],
        [[lua-language-server]]
      })
      do
        local p = mr.get_package(tool)
        if not p:is_installed() then
          p:install()
        end
      end
    end
    if mr.refresh then
      mr.refresh(ensure_installed)
    else
      ensure_installed()
    end
  end,

  opts = {
    max_concurrent_installers = 10,
    pip = {
      upgrade_pip = true
    },
    ui = {
      border = [[single]]
    }
  }
}
