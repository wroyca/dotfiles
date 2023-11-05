local M = {}

---Try to add inlay hints for a specific LSP client and buffer.
---@param client table
-- @param buffer integer
function M.try_add_inlay(client, buffer)
  if client.supports_method([[textDocument/inlayHint]]) then
    pcall(vim.lsp.inlay_hint, buffer, true)
    vim.api.nvim_create_autocmd({ [[InsertEnter]] }, {
      buffer = buffer,
      callback = function()
        pcall(vim.lsp.inlay_hint, buffer, false)
      end
    })
    vim.api.nvim_create_autocmd({ [[InsertLeave]] }, {
      buffer = buffer,
      callback = function()
        pcall(vim.lsp.inlay_hint, buffer, true)
      end
    })
  end
end

---Try to associate an LSP client with buffers of a given filetype.
---@param client string
---@param ft string
function M.try_add_wrapper(client, ft)
  for _, buf in pairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local ftype = vim.api.nvim_get_option_value([[filetype]], { buf = buf })
      if ftype == ft then
        client.manager:try_add_wrapper(buf)
      end
    end
  end
end

---Checks if a string starts with another.
---@param s string
---@param pat string
function M.starts_with(s, pat)
  return s:sub(1, #pat) == pat
end

---Splits a string
---@param s string
---@param pat string
---@return string,string?
function M.split(s, pat)
  local i, j = s:find(pat, nil, true)
  if i ~= nil then
    return s:sub(1, i - 1), s:sub(j + 1)
  else
    return s
  end
end

---Merge lists into a single list.
---Note: This mutates the first list.
---@generic T
---@param dst T[]
---@param t1 T[]?
---@param ... T[]
local function concat(dst, t1, ...)
  if t1 == nil then
    return dst
  end
  for _, t in ipairs(t1) do
    table.insert(dst, t)
  end
  return concat(dst, ...)
end
M.concat = concat

---Merge tables into a signle table.
---Note: This mutates the first table.
---@generic T: table
---@param dst T
---@param t1 T?
---@param ... T
---@return T
local function merge(dst, t1, ...)
  if t1 == nil then
    return dst
  end
  for k, v in pairs(t1) do
    if v == vim.NIL then
      dst[k] = nil
    else
      dst[k] = v
    end
  end
  return merge(dst, ...)
end
M.merge = merge

---Returns whether given table is a list.
---Note: An empty table is not considered as a list.
---@param tbl table
function M.is_list(tbl)
  local i = 1
  for _ in pairs(tbl) do
    if tbl[i] == nil then
      return false
    end
    i = i + 1
  end
  return i > 1
end

local function is_table(t)
  return type(t) == "table" and not M.is_list(t)
end

---Recursively merge tables into a signle table.
---Note: This mutates the first table.
---@generic T: table
---@param dst T
---@param t1 T?
---@param ... T
---@return T
local function deep_merge(dst, t1, ...)
  if t1 == nil then
    return dst
  end
  for k, v in pairs(t1) do
    if is_table(dst[k]) and is_table(v) then
      deep_merge(dst[k], v)
    elseif v == vim.NIL then
      dst[k] = nil
    else
      dst[k] = v
    end
  end
  return deep_merge(dst, ...)
end
M.deep_merge = deep_merge

---Constructs a table from the given table, `false` values will be removed and
---`true` values will be replaced with empty tables.
---@param tbl table<string,table|boolean>
function M.copy_as_table(tbl)
  ---@type table<string,table>
  local ret = {}
  for k, v in pairs(tbl) do
    if type(v) == "table" then
      ret[k] = v
    elseif v then
      ret[k] = {}
    end
  end
  return ret
end

---@param list string[]
function M.list_to_set(list)
  ---@type table<string,boolean>
  local set = {}
  for _, k in ipairs(list) do
    set[k] = true
  end
  return set
end

---@alias util.reducetype "list"|"map"|"table"
---@alias util.reducer table|(fun(dst:table):table)|boolean

---Merges values into a single value.
---Note: this mutates the dst.
---@param f fun(dst:table,new:table):table
---@param dst table
---@param t1 util.Reducer?
---@param ... util.Reducer
---@return table
local function reduce_with(f, dst, t1, ...)
  if t1 == nil then
    return dst
  elseif type(t1) == "table" then
    dst = f(dst, t1)
  elseif type(t1) == "function" then
    dst = t1(dst)
  elseif t1 == false then
    dst = {}
  end
  return reduce_with(f, dst, ...)
end
M.reduce_with = reduce_with

---@param ty util.reduceType
---@param dst table
---@param ... util.reducer
function M.reduce(ty, dst, ...)
  local f
  if ty == "list" then
    f = M.concat
  elseif ty == "map" then
    f = M.merge
  else
    f = M.deep_merge
  end
  return M.reduce_with(f, dst, ...)
end

return M
