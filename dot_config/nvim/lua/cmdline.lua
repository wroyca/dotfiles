-- Forked from https://github.com/Sam-programs/cmdline-hl.nvim

-- Module definition =============================================================

---Module export mechanism
---
---@class Cmdline
---@field config table Module configuration
---@field ns_id integer Namespace ID for UI attachment and highlights
---@field state table Current module state
---@field ts_hl_cache table Cache for treesitter highlighters
---@field handlers table UI handlers for command line events
local Cmdline = {}

--- Module setup
---
---@param config table|nil Module config table. See |Cmdline.config|.
---
---@usage `require('cmdline').setup({})` (replace `{}` with your `config` table)
Cmdline.setup = function(config)
  _G.Cmdline = Cmdline

  config = config or {}

  Cmdline.config = Cmdline.get_config(config)
  Cmdline.apply_config()
  Cmdline.create_autocommands()
  Cmdline.create_mappings()
  Cmdline.enable()
end

-- Module config ================================================================

-- Default values for options
Cmdline.config = {
  -- Highlight groups for command line elements
  hl = {
    -- Command type indicators
    cmdtype = {
      [':'] = { hl = 'Title' },
      ['/'] = { hl = 'Title' },
      ['?'] = { hl = 'Title' },
      ['='] = { hl = 'Title' },
    },
    -- Range highlight
    range = 'Constant',
    -- Prompts and inputs highlight
    input = 'Title',
    -- Ghost text highlight
    ghost_text = 'Comment',
  },

  -- Custom command types with specific highlighting
  custom_commands = {
    ['='] = { pattern = '=(.*)', lang = 'lua', show_cmd = true },
    ['help'] = { },
    ['substitute'] = { lang = 'regex', show_cmd = true },
  },

  -- Command aliases
  aliases = {},

  -- Ghost text settings
  ghost_text = {
    enabled = true,
    provider = nil, -- Function that returns ghost text (nil for history-based)
    inline = false, -- Whether to show ghost text inline or at the end
  },
}

-- Module functionality ==========================================================

-- Namespace for UI attachment and highlights
Cmdline.ns_id = vim.api.nvim_create_namespace('Cmdline')

-- Track state
Cmdline.state = {
  enabled = false,
  ui_attached = false,
  cmdtype = "",
  cmdline = "",
  cursor_pos = -1,
  last_ctx = { prefix = "", cmdline = " ", cursor = -1 },
  ch_before = -1,
  mapping_has_cr = false,
  ghost_text_co = nil,      -- Track the current ghost text coroutine
  last_ghost_text = "",     -- Last displayed ghost text
}

-- Highlighter cache for treesitter
Cmdline.ts_hl_cache = {}

-- Add to state tracking for redraw debouncing
Cmdline.state.redraw_timer = nil      -- Track the debounce timer
Cmdline.state.redraw_pending = false  -- Track if a redraw is already pending

-- UI handlers
Cmdline.handlers = {
  ["cmdline_pos"] = function(cursor, _)
    if not Cmdline.state.enabled then return end
    Cmdline.draw_cmdline(
      Cmdline.state.cmdtype,
      Cmdline.state.cmdline,
      cursor + 1
    )
  end,

  ["cmdline_show"] = function(content, cursor, type, prompt, _, _)
    if not Cmdline.state.enabled then return end
    if Cmdline.state.mapping_has_cr then return end

    cursor = cursor + 1
    if type == "" then
      Cmdline.state.cmdtype = prompt
    else
      Cmdline.state.cmdtype = type
    end

    -- Parse the argument
    local data = ""
    for i = 1, #content, 1 do
      data = data .. content[i][2]
    end

    Cmdline.state.cmdline = data
    Cmdline.state.cursor_pos = cursor
    Cmdline.draw_cmdline(Cmdline.state.cmdtype, data, cursor)
  end,
}

---Get module configuration with user overrides applied
---
---@param config table|nil User configuration
Cmdline.get_config = function(config)
  return vim.tbl_deep_extend('force', Cmdline.config, config or {})
end

---Apply module configuration
Cmdline.apply_config = function()
  -- Create alias mappings
  Cmdline.create_aliases()
end

---Setup command line cursor highlighting
Cmdline.setup_cursor_highlight = function()
  -- Hide Neovim's cursor in cmdline
  vim.api.nvim_set_hl(0, "CmdlineHiddenCursor", { blend = 100, nocombine = true })
  vim.opt_global.guicursor:append({
    "ci:CmdlineHiddenCursor",
    "c:CmdlineHiddenCursor",
    "cr:CmdlineHiddenCursor"
  })
end

---Create autocommands for command line functionality
Cmdline.create_autocommands = function()
  local augroup = vim.api.nvim_create_augroup('Cmdline', { clear = true })

  -- Track CR in mappings
  vim.api.nvim_create_autocmd("CmdlineChanged", {
    group = augroup,
    callback = function()
      Cmdline.state.mapping_has_cr =
        vim.fn.getcharstr(1) == vim.api.nvim_replace_termcodes("<cr>", true, true, true)
    end,
  })

  -- Initialize cmdline tracking
  vim.api.nvim_create_autocmd("CmdlineEnter", {
    group = augroup,
    callback = function()
      Cmdline.state.mapping_has_cr = false
      Cmdline.state.last_ctx.cmdline = "not empty"
    end,
  })

  -- Clean up after cmdline exit
  vim.api.nvim_create_autocmd("CmdlineLeave", {
    group = augroup,
    callback = function()
      if not Cmdline.state.enabled then return end

      pcall(Cmdline.draw_cmdline,
        Cmdline.state.cmdtype,
        Cmdline.state.cmdline,
        -1,
        true
      )

      if Cmdline.state.ch_before ~= -1 then
        vim.o.ch = Cmdline.state.ch_before
      end

      Cmdline.state.ch_before = -1
    end,
  })

  -- Redraw cmdline on window resize
  vim.api.nvim_create_autocmd("VimResized", {
    group = augroup,
    callback = function()
      if not Cmdline.state.enabled then return end

      vim.schedule(function()
        Cmdline.draw_lastcmdline()
      end)
    end,
  })

  -- Re-apply cursor highlight when colorscheme changes
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = augroup,
    callback = function()
      if Cmdline.state.enabled then
        Cmdline.setup_cursor_highlight()
      end
    end,
  })
end

---Create mappings for command line functionality
Cmdline.create_mappings = function()
  -- Add mapping for right arrow to complete ghost text
  vim.keymap.set('c', '<Right>', function()
    -- Check if we're at the end of the line (where ghost text would appear)
    local col = vim.fn.getcmdpos()
    local cmd = vim.fn.getcmdline()
    local cmdtype = vim.fn.getcmdtype()

    -- Only complete if cursor is at the end of the line
    if col > #cmd then
      local ghost_text = Cmdline.get_ghost_text(cmdtype, cmd, col)

      -- If ghost text exists, complete with it
      if ghost_text and #ghost_text > 0 then
        return ghost_text
      end
    end

    -- Otherwise, allow normal right arrow behavior
    return '<Right>'
  end, { expr = true, noremap = true })
end

---Create alias mappings based on configuration
Cmdline.create_aliases = function()
  for cmd, alias in pairs(Cmdline.config.aliases) do
    local key = cmd:sub(#cmd, #cmd)

    vim.keymap.set("c", key, function()
      if vim.fn.getcmdtype() ~= ':' then
        return key
      end

      local cmdline = vim.fn.getcmdline()
      local _, cmd_part = Cmdline.split_range(cmdline)

      for cmd_name, alias_data in pairs(Cmdline.config.aliases) do
        if cmd_name:sub(#cmd_name, #cmd_name) ~= key then
          goto continue
        end

        if #cmd_name == 1 then
          if cmd_part == "" then
            return alias_data.cmd
          else
            return key
          end
        end

        if #cmd_part == #cmd_name - 1 then
          if cmd_part:sub(1, #cmd_name - 1) == cmd_name:sub(1, #cmd_name - 1) then
            return ("<bs>"):rep(#cmd_name - 1) .. alias_data.cmd
          end
        end

        ::continue::
      end

      return key
    end, { expr = true })
  end
end

---Draw command line with highlights
---@param prefix string Command prefix character
---@param cmdline string Command line content
---@param cursor integer|nil Cursor position
---@param force boolean|nil Force redraw even if not in command mode
Cmdline.draw_cmdline = function(prefix, cmdline, cursor, force)
  if vim.fn.getcmdtype() == "" and not force then
    return
  end

  local hl_cmdline = {}
  local ctype = nil
  local render_cursor = cursor

  if prefix == ":" then
    local ok, cmdinfo = pcall(vim.api.nvim_parse_cmd, cmdline, {})
    if not ok then
      cmdinfo = { cmd = "?" }
    end

    local render_cmdline = cmdline
    render_cmdline, render_cursor = Cmdline.process_aliases(cmdinfo, cmdline, cursor)
    hl_cmdline, render_cursor, ctype = Cmdline.highlight_cmdline(cmdinfo, render_cmdline, render_cursor)
  end

  if Cmdline.is_search(prefix) then
    hl_cmdline = Cmdline.highlight_with_ts(cmdline, "regex")
  end

  if prefix == "=" then
    local expr_start = "let a="
    hl_cmdline = Cmdline.highlight_with_ts(expr_start .. cmdline, "vim")
    for _ = 1, #expr_start, 1 do
      table.remove(hl_cmdline, 1)
    end
  end

  if Cmdline.config.hl.cmdtype[prefix] == nil then
    for i = 1, #cmdline, 1 do
      hl_cmdline[i] = { cmdline:sub(i, i) }
    end
  end

  if cursor == -1 then
    goto theend
  end

  if Cmdline.config.ghost_text.enabled then
    if (#hl_cmdline + 1) == render_cursor or Cmdline.config.ghost_text.inline then
      local ghost_text = Cmdline.get_ghost_text(prefix, cmdline, cursor) or ""
      for i = #ghost_text, 1, -1 do
        table.insert(
          hl_cmdline,
          render_cursor,
          { ghost_text:sub(i, i), Cmdline.config.hl.ghost_text }
        )
      end
    end
  end

  -- Highlight cursor
  if render_cursor <= #hl_cmdline then
    local cur_hl = vim.api.nvim_get_hl(
      0,
      { name = hl_cmdline[render_cursor][2], link = false }
    )

    if cur_hl.fg then
      local normal_hl = vim.api.nvim_get_hl(0, { name = "MsgArea", link = false })
      local bg = cur_hl.bg
      cur_hl.bg = cur_hl.fg
      cur_hl.fg = bg or normal_hl.bg
    else
      cur_hl = { link = "Cursor" }
    end

    vim.api.nvim_set_hl(0, "CmdlineCursor", cur_hl)
    hl_cmdline[render_cursor][2] = "CmdlineCursor"
  else
    hl_cmdline[#hl_cmdline + 1] = { " ", "Cursor" }
  end

  ::theend::
  Cmdline.state.last_ctx = { prefix = prefix, cmdline = cmdline, cursor = cursor }

  -- Add command type indicator with the appropriate prefix character
  -- Use a consistent highlight for all command types
  local display_prefix = prefix
  local display_hl = 'Title'  -- Default highlight for all command prefixes

  -- Insert the command type indicator
  table.insert(hl_cmdline, 1, { display_prefix, display_hl })

  -- Adjust command height if needed
  local len = #hl_cmdline[1][1] + #hl_cmdline - 1
  local last_ch = vim.o.ch
  local new_ch = math.ceil((len + 1) / vim.o.columns)

  if last_ch ~= new_ch then
    if Cmdline.state.ch_before == -1 then
      Cmdline.state.ch_before = last_ch
    end
    vim.o.ch = new_ch
    -- Redraw the statusline properly
    vim.schedule(function()
      vim.cmd.redraw()
    end)
  end

  vim.api.nvim_echo(hl_cmdline, false, {})
end

---Draw the last command line
Cmdline.draw_lastcmdline = function()
  Cmdline.draw_cmdline(
    Cmdline.state.last_ctx.prefix,
    Cmdline.state.last_ctx.cmdline,
    Cmdline.state.last_ctx.cursor
  )
end

---Process aliases in command line
---@param cmdinfo table Command info from nvim_parse_cmd
---@param cmdline string Command line content
---@param cursor integer Cursor position
Cmdline.process_aliases = function(cmdinfo, cmdline, cursor)
  local range, cmd = Cmdline.split_range(cmdline)
  local ctype = Cmdline.config.custom_commands[cmdinfo.cmd]

  if not ctype or ctype.show_cmd then
    for render_cmd, alias in pairs(Cmdline.config.aliases) do
      if cmd:sub(1, #alias.cmd) == alias.cmd then
        cursor = cursor + (#render_cmd - #alias.cmd)
        return range .. render_cmd .. cmd:sub(#alias.cmd + 1), cursor
      end
    end
  end

  return cmdline, cursor
end

---Highlight command line with syntax highlighting
---@param cmdinfo table Command info from nvim_parse_cmd
---@param cmdline string Command line content
---@param col integer Cursor position
Cmdline.highlight_cmdline = function(cmdinfo, cmdline, col)
  local retval = {}
  local p_cmd = cmdinfo.cmd
  local ctype = Cmdline.config.custom_commands[p_cmd]
  local range, cmd = Cmdline.split_range(cmdline)
  local code = nil

  if ctype then
    if ctype.code_extractor then
      code = ctype.code_extractor(cmd, cmdinfo)
    else
      code = cmd:match(ctype.pattern or "%w*[%s/]([^|]*)")
    end
  end

  if code then
    retval = Cmdline.highlight_with_ts(code, ctype.lang or "vim")
    local cmd_len = (#cmd - #code)

    if ctype.show_cmd then
      local cmd_tbl = Cmdline.highlight_with_ts(cmd:sub(1, cmd_len), "vim")
      local range_tbl = Cmdline.string_to_hl_tbl(range, Cmdline.config.hl.range)
      retval = Cmdline.merge_tables(range_tbl, cmd_tbl, retval)
    else
      if col ~= -1 then
        if col < #range + cmd_len then
          local cmd_tbl = Cmdline.highlight_with_ts(cmd:sub(1, cmd_len), "vim")
          local range_tbl = Cmdline.string_to_hl_tbl(range, Cmdline.config.hl.range)
          retval = Cmdline.merge_tables(range_tbl, cmd_tbl)
        else
          col = col - #range - cmd_len
        end
      end
    end
  else
    retval = Cmdline.highlight_with_ts(cmdline, "vim")
  end

  return retval, col, code and p_cmd or nil
end

---Highlight text with treesitter
---@param str string String to highlight
---@param language string Language for treesitter
---@param default_hl string|nil Default highlight group
Cmdline.highlight_with_ts = function(str, language, default_hl)
  str = str .. "\n"

  local ret = {}
  for i = 1, #str, 1 do
    ret[i] = { str:sub(i, i), default_hl }
  end

  local priority_list = {}
  local parent_tree = vim.treesitter.get_string_parser(str, language)

  if not parent_tree then return ret end

  parent_tree:parse(true)
  parent_tree:for_each_tree(function(tstree, tree)
    if not tstree then return end

    local lang = tree:lang()
    if Cmdline.ts_hl_cache[lang] == nil then
      Cmdline.ts_hl_cache[lang] = vim.treesitter.query.get(lang, "highlights")
      if Cmdline.ts_hl_cache[lang] == nil then
        return
      end
    end

    local query = Cmdline.ts_hl_cache[lang]
    for id, node, metadata in query:iter_captures(tstree:root(), str, 0, 1, {}) do
      local hl = "@" .. query.captures[id]
      if hl:find("_") then goto continue end

      local _, start_col = node:start()
      local _, end_col = node:end_()

      -- Handle end of line case
      if end_col == 0 then
        end_col = #str
      end

      local priority = 100 + (metadata.priority or 0)
      for i = start_col, end_col - 1, 1 do
        if (priority_list[i + 1] or 0) <= priority then
          ret[i + 1][2] = hl
          priority_list[i + 1] = priority
        end
      end

      ::continue::
    end
  end)

  -- Remove the newline character from result
  ret[#ret] = nil

  return ret
end

---Get ghost text suggestion for current command asynchronously
---@param type string Command type
---@param cmdline string Current command line content
---@param cursor integer Cursor position
Cmdline.get_ghost_text = function(type, cmdline, cursor)
  if Cmdline.state.ghost_text_co then
    -- Cancel previous coroutine if still running
    if coroutine.status(Cmdline.state.ghost_text_co) ~= "dead" then
      Cmdline.state.ghost_text_co = nil
    end
  end

  ---Create a new coroutine for ghost text computation
  Cmdline.state.ghost_text_co = coroutine.create(function()
    local result = ""

    if Cmdline.config.ghost_text.provider then
      result = Cmdline.config.ghost_text.provider(type, cmdline, cursor)
    else
      result = Cmdline.get_history_completion(type, cmdline, cursor)
    end

    -- Only redraw if the ghost text has changed
    if result ~= Cmdline.state.last_ghost_text then
      Cmdline.state.last_ghost_text = result

      -- Schedule UI update only when ghost text has changed
      vim.schedule(function()
        if Cmdline.state.enabled then
          Cmdline.draw_cmdline(
            Cmdline.state.cmdtype,
            Cmdline.state.cmdline,
            Cmdline.state.cursor_pos
          )
        end
      end)
    end

    return result
  end)

  local ok, result = coroutine.resume(Cmdline.state.ghost_text_co)
  if ok and result then
    Cmdline.state.last_ghost_text = result
  end
  return (ok and result) or ""
end

---Get completion suggestion from command history
---@param type string Command type
---@param cmdline string Current command line content
---@param cursor integer Cursor position
Cmdline.get_history_completion = function(type, cmdline, cursor)
  -- Multi-character command types don't support history completion
  if #type > 1 then return "" end

  -- Keep empty command lines empty (no ghost text)
  if #cmdline == 0 then return "" end

  -- For partial cmdline, find matching history
  local pos = cursor - 1
  local prefix = cmdline:sub(1, pos)

  local count = 0
  local should_yield = function()
    count = count + 1
    if count % 100 == 0 then
      coroutine.yield("")
      return true
    end
    return false
  end

  -- Search through history for a matching entry
  for i = vim.fn.histnr(type), 1, -1 do
    local item = vim.fn.histget(type, i)
    should_yield()

    if item:sub(1, pos) == prefix then
      return item:sub(pos + 1, #item)
    end
  end

  return ""
end

---Create an async ghost text provider
---@param provider function The actual ghost text provider function
Cmdline.async_ghost_provider = function(provider)
  return function(type, cmdline, cursor)
    -- Allow yielding every few milliseconds
    local start_time = vim.uv.hrtime()

    local function should_yield()
      -- Yield every 5ms to avoid blocking the UI
      local elapsed = (vim.uv.hrtime() - start_time) / 1000000
      if elapsed > 5 then
        coroutine.yield()
        start_time = vim.uv.hrtime()
        return true
      end
      return false
    end

    -- Call the actual provider with yielding capability
    return provider(type, cmdline, cursor, should_yield)
  end
end

-- Helper functions ===========================================================

---Check if character is a search command
---
---@param char string Character to check
Cmdline.is_search = function(char)
  return char == "/" or char == "?"
end

---Check if character is whitespace
---
---@param char string Character to check
Cmdline.is_whitespace = function(char)
  return char == " " or char == "\t"
end

---Split command line into range and command parts
---@param cmdline string Command line content
Cmdline.split_range = function(cmdline)
  local i = 1
  local in_range_pat = false

  -- Skip leading whitespace
  while Cmdline.is_whitespace(cmdline:sub(i, i)) do
    i = i + 1
  end

  while i <= #cmdline do
    local char = cmdline:sub(i, i)

    if Cmdline.is_search(char) then
      in_range_pat = not in_range_pat
    end

    if in_range_pat then
      goto continue
    end

    if char:match("%w") then
      local prev_char = cmdline:sub(i - 1, i - 1)
      -- If it's not a mark then it's the start of the command
      if prev_char ~= "'" then
        break
      end
    end

    ::continue::
    if not char:match("[%w/\\\\?%%$.<>\'\",+-]") then
      break
    end

    i = i + 1
  end

  return cmdline:sub(1, i - 1), cmdline:sub(i)
end

---Convert string to highlight table
---@param str string String to convert
---@param hl string Highlight group
Cmdline.string_to_hl_tbl = function(str, hl)
  local tbl = {}
  for i = 1, #str, 1 do
    table.insert(tbl, { str:sub(i, i), hl })
  end
  return tbl
end

---Merge highlight tables
---@param ... table Tables to merge
Cmdline.merge_tables = function(...)
  local retval = {}
  for _, tbl in ipairs({ select(1, ...) }) do
    for i = 1, #tbl, 1 do
      retval[#retval + 1] = vim.deepcopy(tbl[i])
    end
  end
  return retval
end

-- API functions ================================================================

---Enable command line highlights
Cmdline.enable = function()
  if Cmdline.state.enabled then return end
  Cmdline.state.enabled = true

  -- Setup cursor highlight
  Cmdline.setup_cursor_highlight()

  -- Attach UI
  if not Cmdline.state.ui_attached then
    vim.ui_attach(Cmdline.ns_id, { ext_cmdline = true }, function(name, ...)
      if Cmdline.handlers[name] then
        xpcall(
          Cmdline.handlers[name],
          function(msg)
            if not msg then return end

            local backtrace = debug.traceback(msg, 1)
            vim.schedule(function()
              vim.notify(
                'Cmdline: Disabling cmdline highlights due to error. ' ..
                'Please report this issue with the backtrace:\n' .. backtrace,
                vim.log.levels.ERROR
              )
              Cmdline.disable()
            end)
          end,
          ...
        )
      end
    end)

    Cmdline.state.ui_attached = true
  end
end

---Disable command line highlights
Cmdline.disable = function()
  if not Cmdline.state.enabled then return end
  Cmdline.state.enabled = false

  -- Restore cursor
  vim.opt_global.guicursor:remove({
    "ci:CmdlineHiddenCursor",
    "c:CmdlineHiddenCursor",
    "cr:CmdlineHiddenCursor"
  })

  -- Detach UI
  if Cmdline.state.ui_attached then
    vim.ui_detach(Cmdline.ns_id)
    Cmdline.state.ui_attached = false
  end
end

--- Toggle command line highlights
Cmdline.toggle = function()
  if Cmdline.state.enabled then
    Cmdline.disable()
  else
    Cmdline.enable()
  end
end

---Create a command line alias
---@param name string Visual command name
---@param replacement string Actual command to execute
Cmdline.create_alias = function(name, replacement)
  Cmdline.config.aliases[name] = { cmd = replacement }
  Cmdline.create_aliases()
end

---Add custom command type
---@param name string Command name
---@param config table Configuration for the command type
Cmdline.add_command_type = function(name, config)
  Cmdline.config.custom_commands[name] = config
end

-- Return module
return Cmdline
