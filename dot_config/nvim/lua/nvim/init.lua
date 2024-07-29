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
    error [[Requires Neovim >= 0.10.0]]
  end
end

--
--

M.config = {}
H.config = vim.deepcopy(M.config)

--
--

return M
