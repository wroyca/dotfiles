local M = {}

M.setup = function()
  local path = vim.fs.joinpath(vim.fn.stdpath [[config]] --[[ @as string]], [[keybindings.json]])
  local file = io.open(path)
  if file then
    local success, json = pcall(vim.json.decode, file:read("*a"), { luanil = { object = true, array = true } })
    if success then
      M.apply_keybindings (json)
    end
    file:close()
  end
end

M.apply_keybindings = function(keybindings)
  if not keybindings then return end
  M.iterate_keybindings(keybindings, M.set_keymap)
end

M.iterate_keybindings = function(keybindings, callback)
  for _, binding in pairs(keybindings) do
    callback(binding)
  end
end

M.set_keymap = function(binding)
  local modes = M.get_modes(binding.mode)
  M.iterate_modes(modes, function(mode)
    M.set_keymap_for_mode(mode, binding.lhs, binding.rhs, binding.opts or {})
  end)
end

M.get_modes = function(mode)
  return type(mode) == [[string]] and { mode } or mode
end

M.iterate_modes = function(modes, callback)
  for _, mode in ipairs(modes) do
    callback(mode)
  end
end

M.set_keymap_for_mode = function(mode, lhs, rhs, opts)
  if M.should_set_keymap(mode, lhs) then
    M.map_key(mode, lhs, rhs, opts)
  end
end

M.should_set_keymap = function(mode, lhs)
  local map_info = M.get_map_info(mode, lhs)
  return M.is_default_keymap(mode, lhs, map_info)
end

M.is_default_keymap = function(mode, lhs, map_info)
  if not map_info then return true end
  if M.is_default_normal_keymap(mode, lhs, map_info) then return true end
  if M.is_default_insert_keymap(mode, lhs, map_info) then return true end
  if M.is_default_visual_keymap(mode, lhs, map_info) then return true end
  return false
end

M.is_default_normal_keymap = function(mode, lhs, map_info)
  local rhs = M.get_rhs(map_info)
  return M.is_normal_mode(mode) and M.is_clear_highlight(lhs, rhs)
end

M.is_default_insert_keymap = function(mode, lhs, map_info)
  local desc = M.get_desc(map_info)
  return M.is_insert_mode(mode) and M.is_signature(lhs, desc)
end

M.is_default_visual_keymap = function(mode, lhs, map_info)
  local rhs = M.get_rhs(map_info)
  return M.is_visual_mode(mode) and M.is_visual_search(lhs, rhs)
end

M.is_normal_mode = function(mode)
  return mode == [[n]]
end

M.is_insert_mode = function(mode)
  return mode == [[i]]
end

M.is_visual_mode = function(mode)
  return mode == [[x]]
end

M.is_clear_highlight = function(lhs, rhs)
  return lhs == [[<C-L>]] and rhs:find [[nohl]] ~= nil
end

M.is_signature = function(lhs, desc)
  return lhs == [[<C-S>]] and desc:find [[signature]] ~= nil
end

M.is_visual_search = function(lhs, rhs)
  return (lhs == '*' and rhs == [[y/\V<C-R>"<CR>]]) or (lhs == '#' and rhs == [[y?\V<C-R>"<CR>]])
end

M.get_map_info = function(mode, lhs)
  local keymaps = vim.api.nvim_get_keymap(mode)
  return M.find_keymap(keymaps, lhs)
end

M.find_keymap = function(keymaps, lhs)
  for _, info in ipairs(keymaps) do
    if info.lhs == lhs then return info end
  end
  return nil
end

M.get_rhs = function(map_info)
  return map_info.rhs or [[]]
end

M.get_desc = function(map_info)
  return map_info.desc or [[]]
end

M.map_key = function(mode, lhs, rhs, opts)
  if lhs == [[]] then return end
  local options = M.merge_options(opts)
  vim.keymap.set(mode, lhs, rhs, options)
end

M.merge_options = function(opts)
  return vim.tbl_deep_extend([[force]], { silent = true }, opts or {})
end

return M
