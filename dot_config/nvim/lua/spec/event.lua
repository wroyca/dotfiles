local event = require("lazy.core.handler.event")

--- Triggers a specified Neovim user-defined event.
--- @param event string: The event name (e.g., "User")
--- @param pattern string: The event pattern to match
--- @param modeline boolean: If modeline should be applied
local function trigger_event(event, pattern, modeline)
  vim.schedule(function()
    vim.api.nvim_exec_autocmds(event, { pattern = pattern, modeline = modeline })
  end)
end

--- Executes the specified buffer-related event.
--- @param args table: Arguments passed to the buffer-related event callback
local function trigger_buffer_event(args)
  vim.schedule(function()
    vim.api.nvim_exec_autocmds(args.event, { buffer = args.buf, data = args.data })
  end)
end

--- Handles execution of the registered event, including optional asynchronous scheduling.
--- @param args table|nil: Arguments passed to the autocmd callback, or nil if not using autocmd
--- @param event_id string: The custom event identifier
--- @param custom_callback function|nil: Optional custom callback for handling event execution
local function handle_event(args, event_id, custom_callback)
  vim.schedule(function()
    if args and args.buf and not vim.api.nvim_buf_is_loaded(args.buf) then return end
    if custom_callback then
      custom_callback(args, event_id)
    else
      trigger_event("User", event_id, false)
      if args then vim.api.nvim_del_autocmd(args.id) end
      if args then trigger_buffer_event(args) end
    end
  end)
end

--- Register a custom event with optional asynchronous handling via autocmd or custom triggers.
--- @param event_id string: Unique identifier for the event (e.g., "AsyncFileLoad")
--- @param desc string: Description of the event's purpose
--- @param opts table: Options for configuring the event
---        - file_events (table|nil): List of file-related events to trigger the custom event (e.g., {"BufReadPost", "BufNewFile"})
---        - callback (function|nil): Custom callback function for handling the event if no autocmd is used
local function register_custom_event(event_id, desc, opts)
  -- Register event in the event mappings if not already present
  if not event.mappings[event_id] then
    event.mappings[event_id] = {
      id = event_id,
      event = "User",
      pattern = event_id,
    }
  end

  -- Define an autocmd if file events are provided
  if opts.file_events then
    local group_name = "custom_plugin_events"
    vim.api.nvim_create_augroup(group_name, { clear = true })

    vim.api.nvim_create_autocmd(opts.file_events, {
      group = group_name,
      desc = desc,
      nested = true,
      callback = function(args)
        handle_event(args, event_id, opts.callback)
      end,
    })
  elseif opts.callback then
    -- Directly invoke the custom callback if no file events are specified
    opts.callback(event_id)
  end
end

register_custom_event("AsyncFileLoad", "Handle file load asynchronously", { file_events = {"BufReadPost", "BufNewFile"}, callback = nil })

return {
}
