---@class key.options
---@field buffer? integer
---@field noremap? boolean
---@field remap? boolean
---@field nowait? boolean
---@field silent? boolean
---@field script? boolean
---@field expr? boolean
---@field replace_keycodes? boolean
---@field unique? boolean
---@field desc? string

---@alias key.presets key.preset[]|key.options

---@alias key.with fun(src:key.input):any

---@class key.preset: key.options
---Preset name
---@field [1] string
---Mapped value
---@field [2] any
---Description
---@field [3]? string
---@field mode? string|string[]
---@field with? key.with
---Default key to be set
---@field key? string
---Variant name
---@field variant? string

---@alias keys key[]|key.options

---@alias key customkey|presetkey

---@class customkey: basekey
---Key
---@field [1] string
---Callback function or command
---@field [2] string|fun()

---@class presetkey: basekey
---Key or boolean value to enable a preset
---@field [1] string|boolean
---Preset name starts with '@'
---@field [2] string
---@field variant? string

---@class basekey: key.options
---Description
---@field [3]? string
---@field mode? string|string[]

---@class key.output
---Key
---@field [1] string
---Mapped value
---@field [2] any
---@field mode string[]
---@field opts key.options

---@alias key.inputs key.input[]|{visited?:boolean}

---@class key.input
---Key
---@field [1]? string
---Preset name
---@field [2] string
---@field mode string[]
---@field desc? string
---@field variant? string

local M = {}
local util = require("detail.util")

---@type string[]
---
M.key_mode = { "n" }

---@type table<string,{[1]:any}>
---
M.key_opts = {
  buffer = {},
  noremap = { true },
  remap = { false },
  nowait = {},
  silent = { true },
  script = {},
  expr = {},
  replace_keycodes = {},
  unique = {},
  desc = {},
}

---sets a key.
---
---@param mode string|string[]
---@param lhs string
---@param rhs string|fun()
---
---@param opts? key.options
---
function M.key(mode, lhs, rhs, opts)
  opts = opts or {}
  local o = {}
  for k, v in pairs(M.key_opts) do
    o[k] = opts[k] or v[1]
  end
  vim.keymap.set(mode, lhs, rhs, o)
end

---Preset inputs shared by all collectors.
---
---@type table<string,key.inputs>
---
local input = {}

---@param preset string
---@return key.inputs?
---
local function get(preset)
  local ret = input[preset]
  if ret then
    ret.visited = true
  end
  return ret
end

---Collects modes.
---
---@param mode string|string[]|nil
---
local function get_mode(mode)
  if type(mode) == "string" then
    return { mode }
  else
    return mode or M.key_mode
  end
end

---Collects options.
---
---@param src table
---@param init? key.options
---
local function get_opts(src, init)
  ---@type key.options
  local opts = init or {}
  for k in pairs(M.key_opts) do
    opts[k] = opts[k] or src[k]
  end
  return opts
end

---@param mapping key.input
---
local function add_input(mapping)
  local name = mapping[2]
  if not input[name] then
    input[name] = {}
  end
  table.insert(input[name], mapping)
end

---@param name string
---
local function remove_input(name)
  input[name] = nil
end

---Collects and maps keys.
---
---@class key.collector
---@field private _output key.output[]
---table<name,table<mode,preset>>
---
local collector = {}

function collector.new()
  local self = setmetatable({ _output = {} }, { __index = collector })
  return self
end

---@param output key.output
---
function collector:add(output)
  table.insert(self._output, output)
  return self
end

---@private
---@param preset key.preset
---@param input key.input
---
---@diagnostic disable-next-line: redefined-local
function collector:_map_preset(preset, input)
  ---check if variant matches.
  ---
  if preset.variant ~= input.variant then
    return
  end
  ---Select common modes.
  ---@type string[]
  ---
  local mode
  local pmode = preset.mode
  ---If no modes are specified, uses modes defined by the preset.
  ---
  if #input.mode == 0 then
    if not pmode or pmode == "*" then
      mode = { "n" }
    else
      mode = {}
      for _, m in ipairs(get_mode(pmode)) do
        table.insert(mode, m == "*" and "n" or m)
      end
    end
    ---If the preset supports all modes, then use input modes.
    ---
  elseif not pmode or pmode == "*" then
    mode = input.mode
    ---Otherwise, only common modes will be selected.
    ---
  else
    local supported = util.list_to_set(get_mode(pmode))
    if supported["*"] then
      mode = input.mode
    else
      mode = {}
      for _, m in ipairs(input.mode) do
        if supported[m] then
          table.insert(mode, m)
        end
      end
      if #mode == 0 then
        return
      end
    end
  end
  ---Generate output.
  ---
  local opts = get_opts(preset, { desc = input.desc or preset[3] })
  local rhs
  if preset.with then
    rhs = preset.with(input)
  else
    rhs = preset[2]
  end
  self:add({
    input[1] or preset.key,
    rhs,
    mode = mode,
    opts = opts,
  })
end

---Converts preset inputs to the output.
---
---@param presets key.presets
---
function collector:map(presets)
  local opts = get_opts(presets)
  for _, preset in ipairs(presets) do
    local name, variant = util.split(preset[1], ":")
    preset = util.merge({}, opts, { variant = variant }, preset)
    ---@diagnostic disable-next-line: redefined-local
    for _, input in ipairs(get(name) or {}) do
      self:_map_preset(preset, input)
    end
  end
  return self
end

---Similar as `collector:map`, but more than one inputs will be ignored.
---
---@param presets key.presets
---
function collector:map_unique(presets)
  local opts = get_opts(presets)
  for _, preset in ipairs(presets) do
    local name, variant = util.split(preset[1], ":")
    preset = util.merge({}, opts, { variant = variant }, preset)
    ---@diagnostic disable-next-line: redefined-local
    local input = get(name)
    if input then
      if #input > 1 then
        vim.log.warn("only the first key of '%s' will be set.", preset[1])
      end
      self:_map_preset(preset, input[1])
    end
  end
  return self
end

---Collects mappings from presets.
---
function collector:collect()
  local ret = self._output
  self._output = {}
  return ret
end

---@param opts? key.options
---
function collector:collect_and_set(opts)
  M.set(self:collect(), opts)
end

---Constructs a list of tables that could be used as keys of `lazy.nvim`.
---
---@param init? table[]
---
function collector:collect_lazy(init)
  local keys = init or {}
  for _, key in ipairs(self:collect()) do
    table.insert(
      keys,
      util.merge({
        key[1],
        key[2],
        mode = key.mode,
      }, key.opts)
    )
  end
  return keys
end

---Constructs a table of mapped values indexed by the input keys.
---
---@param init? table<string,any>
---
function collector:collect_lhs_table(init)
  local tbl = init or {}
  for _, key in ipairs(self:collect()) do
    tbl[key[1]] = key[2]
  end
  return tbl
end

---Constructs a table of input keys indexed by mapped values.
---
---@param init? table<any,string>
---
function collector:collect_rhs_table(init)
  local tbl = init or {}
  for _, key in ipairs(self:collect()) do
    tbl[key[2]] = key[1]
  end
  return tbl
end

---Loads keys. Preset inputs will be added to the global storage and other
---
---keys will be extended into the returned collector.
---@param keys keys
---
function M.load(keys)
  ---@diagnostic disable-next-line: redefined-local
  local collector = collector.new()
  local opts = get_opts(keys)
  for _, mapping in ipairs(keys) do
    ---@type key
    ---
    mapping = util.merge({}, mapping, opts)
    local key = mapping[1]
    local rhs = mapping[2]
    local desc = mapping[3] or mapping.desc
    if type(rhs) == "string" and util.starts_with(rhs, "@") then
      if key == false then
        remove_input(rhs)
      else
        ---@type string?
        ---
        local k
        if key == true then
          k = nil
        else
          k = key --[[@as string]]
        end
        local r, v = util.split(rhs, ":")
        add_input({
          ---@diagnostic disable-next-line:assign-type-mismatch
          ---
          k,
          r,
          mode = get_mode(mapping.mode or {}),
          desc = desc,
          variant = v or mapping.variant,
        })
      end
    elseif type(key) == "string" then
      collector:add({
        key,
        rhs,
        mode = get_mode(mapping.mode),
        opts = get_opts(mapping, { desc = desc }),
      })
    end
  end
  return collector
end

---Sets keys.
---
---@param keys key.output[]
---@param opts? key.options
---
function M.set(keys, opts)
  for _, key in ipairs(keys) do
    M.key(key.mode, key[1], key[2], util.merge({}, opts or {}, key.opts))
  end
end

---@param cb? fun(name:string,visited:boolean)
---
function M.check(cb)
  cb = cb
      ---@param name string
      ---@param visited boolean
      ---
      or function(name, visited)
        if not visited then
          vim.health.report_warn(("key `%s` is not applied"):format(name))
        end
      end
  for k, v in pairs(input) do
    cb(k, v.visited == true)
  end
end

M.collector = collector.new

return M
