---@type LazyPluginSpec
return {
  [[williamboman/mason.nvim]],
  name = [[mason]],
  build = function()
    ---@diagnostic disable-next-line: undefined-field
    require [[mason]].setup()
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
  config = true
}
