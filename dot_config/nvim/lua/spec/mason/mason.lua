---@type LazyPluginSpec
return {
  [[mason]],

  build = function()
    local _ = require [[mason]]
    local r = require [[mason-registry]]

    local function ensure_installed()
      for _, tool in ipairs({
        [[clangd]],
        [[lua-language-server]]
      })
      do
        local p = r.get_package(tool)
        if not p:is_installed() then
          p:install()
        end
      end
    end
    if r.refresh then
      r.refresh(ensure_installed)
    else
      ensure_installed()
    end
  end,

  opts = {
    max_concurrent_installers = 10,
    pip = {
      upgrade_pip = true
    }
  }
}
