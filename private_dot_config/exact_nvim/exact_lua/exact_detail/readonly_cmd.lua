---@class cmd.options
---@field group? string|integer
---@field buffer? integer
---@field pattern? string|string[]
---@field once? boolean
---@field nested? boolean
---@field desc? string

---@alias cmd.callback fun(ev:cmd.event)

---@class cmd.event
---@field id number
---@field number string
---@field group? number
---@field match string
---@field buf number
---@field file string
---@field data any

---@alias cmds cmd[]|cmd.options

---@alias cmd customcmd|presetcmd

---@class customcmd: basecmd
---Events
---@field [1] string|string[]
---Command, callback function or boolean value to enable a preset
---@field [2] string|cmd.callback

---@class presetcmd: basecmd
---Preset name starts with '@'
---@field [1] string
---Boolean value to enable a preset
---@field [2] boolean

---@class basecmd: cmd.options
---Description
---@field [3]? string

---@alias cmd.presets cmd.preset[]|cmd.options

---@alias cmd.with fun(src:cmd.input):customcmd|customcmd[]|{grouped?:boolean|string}

---@alias cmd.schema table<string,util.reducetype>

---@class cmd.preset: keymap.options
---Preset name
---@field [1] string
---Events
---@field [2]? string|string[]
---Callback or command
---@field [3]? cmd.callback|string
---@field with? cmd.with
---@field args? cmd.schema
--
---@class cmd.output
---Events
---@field [1] string|string[]
---Command or callback function
---@field [2] string|cmd.callback
---@field opts cmd.options

---@alias cmd.inputs cmd.input[]|{visited?:boolean}

---@class cmd.input
---Preset name
---@field [1] string
---@field [2] boolean
---@field desc? string
---@field args table<string,any>

local M = {}
local util = require("detail.util")

---@type table<string,{[1]:any}>
---
M.cmd_opts = {
  group = {},
  buffer = {},
  pattern = {},
  once = {},
  nested = {},
  desc = {},
}

---Sets a cmd.
---
---@param events string|string[]
---@param cmd string|cmd.callback
---@param opts? cmd.options
---
function M.cmd(events, cmd, opts)
  opts = opts or {}
  local o = {}
  if type(cmd) == "string" then
    o.command = cmd
  else
    o.callback = cmd
  end
  for k, v in pairs(M.cmd_opts) do
    o[k] = opts[k] or v[1]
  end
  vim.api.nvim_create_autocmd(events, o)
end

---Collects options and arguments.
---
---@param src table
---@param init? cmd.options
---
local function get_opts(src, init)
  ---@type cmd.options
  ---
  local opts = init or {}
  for k in pairs(M.cmd_opts) do
    opts[k] = opts[k] or src[k]
  end
  return opts
end

---Collects arguments.
---
---@param src table
---
local function get_args(src)
  ---@type table<string,any>
  ---
  local args = {}
  for k, v in pairs(src) do
    if type(k) == "string" and M.cmd_opts[k] == nil then
      args[k] = v
    end
  end
  return args
end

---Preset inputs shared by all collectors.
---
---@type table<string,cmd.inputs>
---
local inputs = {}

---@param preset string
---
---@return cmd.inputs?
---
local function get(preset)
  local ret = inputs[preset]
  if ret then
    ret.visited = true
  end
  return ret
end

---@param input cmd.input
---
local function add_input(input)
  local name = input[1]
  if inputs[name] == nil then
    inputs[name] = {}
  end
  table.insert(inputs[name], input)
end

---@param name string
---
local function remove_input(name)
  inputs[name] = nil
end

---@class cmd.collector
---@field private _output cmd.output[]
---
local collector = {}

function collector.new()
  local self = setmetatable({ _output = {} }, { __index = collector })
  return self
end

---@param cmd cmd.output
---
function collector:add(cmd)
  table.insert(self._output, cmd)
  return self
end

---@private
---@param preset cmd.preset
---
function collector:_map_preset(preset)
  local input = get(preset[1])
  if input == nil then
    return self
  end
  ---Merge input tables.
  ---
  ---@type cmd.input
  ---
  local desc
  local args = {}
  ---@type cmd.schema
  ---
  local schema = preset.args or {}
  for _, inp in ipairs(input) do
    desc = desc or inp.desc
    ---Reduce arguments
    ---
    local new = inp.args
    for k, t in pairs(schema) do
      args[k] = util.reduce(t, args[k] or {}, new[k] or {})
    end
  end
  ---Collect output tables from custom function
  ---
  local src = { preset[1], desc = desc, args = args }
  if preset.with then
    local output = preset.with(src)
    if output.grouped then
      local group
      if type(output.grouped) == "string" then
        group = vim.api.nvim_create_augroup(output.grouped --[[@as string]], {})
      end
      ---@cast output customcmd[]
      ---
      for _, o in ipairs(output) do
        self:add({
          o[1],
          o[2],
          opts = util.merge({ group = group }, get_opts(o)),
        })
      end
    else
      ---@cast output customcmd
      ---
      self:add({
        output[1],
        output[2],
        opts = get_opts(output),
      })
    end
  ---Or construct from preset directly.
  ---
  elseif preset[2] then
    self:add({
      preset[2],
      preset[3],
      opts = get_opts(src),
    })
  end
  return self
end

---Constructs cmds from preset inputs.
---
---@param presets cmd.presets
---
function collector:map(presets)
  local opts = get_opts(presets)
  for _, preset in ipairs(presets) do
    self:_map_preset(util.merge({}, preset, opts))
  end
  return self
end

---Collects output cmds.
---
function collector:collect()
  local ret = self._output
  self._output = {}
  return ret
end

function collector:collect_and_set()
  M.set(self:collect())
end

---Loads auto commands.
---
---@param cmds cmds
---
function M.load(cmds)
  ---@diagnostic disable-next-line: redefined-local
  local collector = collector.new()
  local gopts = get_opts(cmds)
  for _, cmd in ipairs(cmds) do
    cmd = util.merge({}, cmd, gopts)
    local event = cmd[1]
    local command = cmd[2]
    local desc = cmd[3] or cmd.desc
    if type(event) == "string" and util.starts_with(event, "@") then
      if command == false then
        remove_input(event)
      else
        add_input({
          event,
          desc = desc,
          args = get_args(cmd),
        })
      end
    elseif type(command) ~= "boolean" then
      collector:add({
        event,
        command,
        opts = get_opts(cmd, { desc = desc }),
      })
    end
  end
  return collector
end

---Sets cmds.
---
---@param cmds cmd.output[]
---
function M.set(cmds)
  for _, cmd in ipairs(cmds) do
    M.cmd(cmd[1], cmd[2], cmd.opts)
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
        vim.health.report_warn(("cmd `%s` is not applied"):format(name))
      end
    end
  for k, v in pairs(inputs) do
    cb(k, v.visited == true)
  end
end

M.collector = collector.new

return M

