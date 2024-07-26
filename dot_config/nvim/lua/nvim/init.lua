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

H.std = function()
  return vim.fn.stdpath [[config]] --[[ @as string ]]
end

H.options = function()
  return vim.fs.joinpath(H.std(), [[options.json]])
end

--
--

H.apply_options = function ()
  return H.iterate_options(H.parse(H.options()), H.set_option) ~= nil
end

H.iterate_options = function(options, callback)
  for option, value in pairs(options) do
    callback(option, value)
  end
end

--
--

H.set_option = function (option, value)
  if not H.is_option(option) then
    return H.set_option_local(option, value)
  end
  if H.is_global_option(option) then
    return H.set_option_schedule(vim.o, option, value)
  end
end

H.is_buffer_option = function(option)
  return vim.api.nvim_get_option_info2(option, {}).scope == [[buf]]
end

H.is_window_option = function(option)
  return vim.api.nvim_get_option_info2(option, {}).scope == [[win]]
end

H.is_global_option = function(option)
  return vim.api.nvim_get_option_info2(option, {}).scope == [[global]]
end

H.is_option = function (option)
  local _, info = pcall(vim.api.nvim_get_option_info2, option, {})
  return info.name ~= nil
end

H.set_option_local = function(filetype, options)
  -- TODO: Validation mechanism to confirm that the specified filetype exists
  -- and to prevent erroneous or invalid input.
  --
  if type(options) == [[table]] then
    H.create_autocmd([[FileType]], filetype, function()
      print ("foo")
      for option, value in pairs(options) do
        if H.is_buffer_option(option) then H.set_buffer_option(option, value) end
        if H.is_window_option(option) then H.set_window_option(option, value) end
      end
    end, false)
  end
end

H.set_buffer_option = function(key, value)
  vim.bo[key] = value
end

H.set_window_option = function(key, value)
  vim.opt_local[key] = value
end

-- Delay setting the option until Nvim has fully started to ensure the
-- OptionSet event triggers.
H.set_option_schedule = function(option_table, key, value)
  local was_set = vim.api.nvim_get_option_info2(key, {}).was_set
  if was_set then return end
  if H.is_vim_entered() then
    option_table[key] = value
  else
    H.create_autocmd([[VimEnter]], "*", function()
      option_table[key] = value
    end, true)
  end
end

H.is_vim_entered = function()
  return vim.v.vim_did_enter == 1
end

H.create_autocmd = function(event, pattern, callback, once)
  vim.api.nvim_create_autocmd(event, {
    pattern = pattern, callback = callback, once = once, nested = true
  })
end

--
--

H.parse = function(path)
  local file = H.open (path)
  local data = H.read (file)

  H.close(file)

  return H.generalize(data)
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
  return io.open(path, [[r]])
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
