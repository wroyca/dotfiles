local M = {}
local H = {}

--
--

M.setup = function(config)
  vim.validate({ config = { config, [[table]], true } })

  config = H.setup(config)

  H.apply(config)
end

H.setup = function(config)
  vim.validate({ config = { config, [[table]], true } })

  config = vim.tbl_deep_extend([[force]], vim.deepcopy(H.config), config or {})

  return config
end

--
--

H.apply = function(config)
  M.config = config

  if vim.fn.has [[nvim-0.10.0]] ~= 1 then
    error [[Neovim >= 0.10.0]]
  end

  local fennel = require [[fennel]]
  table.insert(package.loaders, fennel.searcher) ---@diagnostic disable-line: deprecated
  table.insert(package.loaders, H.apply_runtime) ---@diagnostic disable-line: deprecated
end

--
--

H.apply_runtime = function(name)
  local basename = name:gsub('%.', '/')
  local paths = {
    [[fnl/]] .. basename .. [[.fnl]],
  }

  for _, path in ipairs(paths) do
    local found = vim.api.nvim_get_runtime_file(path, false)
    if #found > 0 then
      return function()
        return require [[fennel]].dofile(found[1])
      end
    end
  end
end

--
--

M.config = {}
H.config = vim.deepcopy(M.config)

--
--

return M
