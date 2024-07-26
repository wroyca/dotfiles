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

  H.apply_options()
end

--
--

H.config = function()
  return vim.fn.stdpath [[config]]
end

H.options = function()
  return vim.fs.joinpath(H.config, [[options.json]])
end

--
--

H.apply_options = function ()
  return H.iterate_options(H.parse(H.options), H.set_option) or nil
end

H.iterate_options = function(options, callback)
  for option, value in pairs(options) do
    callback(option, value)
  end
end

H.set_option = function () end

--
--

H.parse = function(path)
  local file = H.open (path)
  local data = H.read (file)
  return H.close(file) and H.generalize(data) or nil
end

H.generalize = function(data)
  local ok, json = pcall(vim.json.decode, data, {
    luanil = {
      object = true,
      array = true
    }
  })
  return ok and json or nil
end

--
--

H.open = function(path)
  return io.open(path, "r")
end

H.read = function(file)
  return file and file:read("*a") or nil
end

H.close = function(file)
  if file then file:close() end
end

--
--

M.config = {}
H.config = vim.deepcopy(M.config)

--
--

return M
