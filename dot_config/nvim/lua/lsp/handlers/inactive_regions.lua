---@class InactiveRegionsModule
---@field config InactiveRegionsConfig
---@field ns integer Namespace ID for highlights
---@field _state InactiveRegionsState Internal state
local InactiveRegions = {}

---@class InactiveRegionsConfig
---@field opacity number Opacity level for inactive regions (0.0-1.0)
---@field namespace string? Custom namespace name
---@field debounce_ms integer Debounce delay for updates in milliseconds
---@field max_chunk_size integer Maximum nodes to process per coroutine yield
---@field precompute_theme boolean Whether to precompute theme color blends
---@field performance_monitoring boolean Enable performance metrics collection
---@field debug boolean Enable debug logging and commands
---@field live_typing boolean Enable live typing blending for inactive regions
---@field typing_debounce_ms integer Debounce delay for live typing updates
---@field word_boundary_update boolean Enable word boundary detection and treesitter analysis
---@field immediate_char_highlight boolean Highlight characters as they're typed
---@field boundary_chars string Characters that are considered word boundaries
local default_config = {
  opacity = 0.5,
  namespace = nil,
  debounce_ms = 16,
  max_chunk_size = 200,
  precompute_theme = true,
  performance_monitoring = false,
  debug = false,
  live_typing = false, -- poc, not relablie, bugs expected.
  typing_debounce_ms = 0,
  word_boundary_update = true,
  immediate_char_highlight = true,
  boundary_chars = " \t\n",
}

---@class InactiveRegionsState
---@field color_cache table<string, string> RGB/Hex color conversion cache
---@field blend_cache table<string, string> Precomputed color blends
---@field highlight_cache table<string, integer> Highlight group cache
---@field region_cache table<string, InactiveRegion[]> Previous regions per buffer
---@field pending_updates table<string, boolean> Buffers with pending updates
---@field debounce_timers table<string, integer> Active debounce timers
---@field active_coroutines table<string, thread> Running processing coroutines
---@field performance_metrics InactiveRegionsMetrics Performance data
---@field theme_signature string Current theme identifier for cache invalidation
---@field live_typing_state InactiveRegionsLiveTypingState Live typing state
local InactiveRegionsState = {}

---@class InactiveRegionsMetrics
---@field total_regions_processed integer
---@field total_highlights_applied integer
---@field cache_hits integer
---@field cache_misses integer
---@field avg_processing_time_ms number
---@field last_update_time_ms number
local InactiveRegionsMetrics = {}

---@class InactiveRegion
---@field start {line: integer, character: integer}
---@field end {line: integer, character: integer}

---@class TypingEvent
---@field bufnr integer Buffer number
---@field lnum integer Line number
---@field col integer Column number
---@field timestamp integer Event timestamp

-- Private Implementation
--

local H = {}

-- Module Lifecycle Management
--

---@param config InactiveRegionsConfig? Optional configuration overrides
function InactiveRegions.setup(config)
  config = H.validate_and_merge_config(config or {})

  InactiveRegions.config = config
  InactiveRegions.ns = vim.api.nvim_create_namespace(config.namespace or "inactive_regions_enterprise")
  InactiveRegions._state = H.create_initial_state()

  H.setup_lsp_handlers()
  H.setup_autocommands()
  H.create_default_highlights()

  if config.debug then
    H.setup_debug_commands()
  end

  if config.precompute_theme then
    H.precompute_theme_blends()
  end

  _G.InactiveRegions = InactiveRegions

  H.log("Inactive regions module initialized successfully")
end

---Clean shutdown of the module
function InactiveRegions.teardown()
  local state = InactiveRegions._state
  if not state then return end

  -- Stops all active debounce timers to prevents pending updates from executing
  -- after teardown.
  for _, timer in pairs(state.debounce_timers) do
    if timer then
      vim.fn.timer_stop(timer)
    end
  end

  -- Clears the table of active coroutines. Note that While this does not stop
  -- running coroutines immediately, it prevents them from being rescheduled.
  state.active_coroutines = {}

  -- Iterates through the region cache, which tracks buffers managed by the
  -- module and removes all highlights applied from all known buffers.
  for filename in pairs(state.region_cache) do
    if vim.fn.bufexists(filename) ~= 0 then
      local bufnr = vim.fn.bufnr(filename)
      vim.api.nvim_buf_clear_namespace(bufnr, InactiveRegions.ns, 0, -1)
    end
  end

  H.log("Inactive regions module shut down")
end

-- Public API
--

---Enable inactive regions for a specific buffer
---@param bufnr integer? Buffer number (defaults to current buffer)
function InactiveRegions.enable(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_var(bufnr, 'inactive_regions_enabled', true)
  H.log("Enabled inactive regions for buffer %d", bufnr)
end

---Disable inactive regions for a specific buffer
---@param bufnr integer? Buffer number (defaults to current buffer)
function InactiveRegions.disable(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(bufnr, InactiveRegions.ns, 0, -1)
  vim.api.nvim_buf_set_var(bufnr, 'inactive_regions_enabled', false)
  H.log("Disabled inactive regions for buffer %d", bufnr)
end

---Toggle debug mode
function InactiveRegions.toggle_debug()
  InactiveRegions.config.debug = not InactiveRegions.config.debug
  if InactiveRegions.config.debug then H.setup_debug_commands() end
  H.log("Debug mode %s", InactiveRegions.config.debug and "enabled" or "disabled")
end

---Clear all caches and force regeneration
function InactiveRegions.invalidate_caches()
  local state = InactiveRegions._state
  if not state then return end

  state.color_cache = {}
  state.blend_cache = {}
  state.highlight_cache = {}
  state.theme_signature = ""

  -- If theme precomputation is enabled, re-runs the precomputation process so
  -- that caches are repopulated according to the current theme.
  if InactiveRegions.config.precompute_theme then
    H.precompute_theme_blends()
  end

  H.log("All caches invalidated and regenerated")
end

---Get performance metrics
---@return InactiveRegionsMetrics
function InactiveRegions.get_metrics()
  return vim.deepcopy(InactiveRegions._state.performance_metrics)
end

-- Configuration and Validation
--

---Validate and merge user configuration with defaults
---@param user_config table User-provided configuration
---@return InactiveRegionsConfig
function H.validate_and_merge_config(user_config)
  local config = vim.tbl_deep_extend("force", default_config, user_config)

  if config.opacity < 0 or config.opacity > 1 then
    vim.notify(
      "InactiveRegions: opacity must be between 0.0 and 1.0, got " .. config.opacity,
      vim.log.levels.WARN
    )
    config.opacity = 0.5
  end

  if config.debounce_ms < 0 or config.debounce_ms > 1000 then
    vim.notify(
      "InactiveRegions: debounce_ms must be between 0 and 1000, got " .. config.debounce_ms,
      vim.log.levels.WARN
    )
    config.debounce_ms = 16
  end

  if config.max_chunk_size < 50 or config.max_chunk_size > 1000 then
    vim.notify(
      "InactiveRegions: max_chunk_size must be between 50 and 1000, got " .. config.max_chunk_size,
      vim.log.levels.WARN
    )
    config.max_chunk_size = 200
  end

  return config
end

---Create initial state structure
---@return InactiveRegionsState
function H.create_initial_state()
  return {
    color_cache = {},
    blend_cache = {},
    highlight_cache = {},
    region_cache = {},
    pending_updates = {},
    debounce_timers = {},
    active_coroutines = {},
    performance_metrics = {
      total_regions_processed = 0,
      total_highlights_applied = 0,
      cache_hits = 0,
      cache_misses = 0,
      avg_processing_time_ms = 0,
      last_update_time_ms = 0,
    },
    theme_signature = "",
    live_typing_state = {
      typing_timers = {},
      cursor_in_inactive = {},
      last_typed_pos = {},
      temp_highlights = {},
    },
  }
end

-- LSP Integration
--

---Set up LSP handlers for inactive regions
function H.setup_lsp_handlers()
  vim.lsp.handlers["textDocument/inactiveRegions"] = H.handle_inactive_regions_request
end

---Handle inactive regions LSP notification with debouncing and async processing
---@param _ any LSP client (unused)
---@param message table LSP message containing regions data
---@param _ any Context (unused)
---@param _ any Config (unused)
function H.handle_inactive_regions_request(_, message, _, _)
  local start_time = vim.loop.hrtime()
  local uri = message.textDocument.uri
  local filename = vim.uri_to_fname(uri)
  local regions = message.regions or {}

  H.log("Received inactive regions request for %s with %d regions", filename, #regions)

  if not H.is_buffer_valid_and_enabled(filename) then
    H.log("Buffer %s is not valid or inactive regions are disabled", filename)
    return
  end

  if not H.regions_changed(filename, regions) then
    H.log("Regions unchanged for %s, skipping update", filename)
    return
  end

  H.cancel_pending_update(filename)
  InactiveRegions._state.region_cache[filename] = vim.deepcopy(regions)
  H.schedule_debounced_update(filename, regions, start_time)
end

---Check if buffer is valid and has inactive regions enabled
---@param filename string Buffer filename
---@return boolean
function H.is_buffer_valid_and_enabled(filename)
  if vim.fn.bufexists(filename) == 0 then
    return false
  end

  local bufnr = vim.fn.bufnr(filename)
  local ok, enabled = pcall(vim.api.nvim_buf_get_var, bufnr, 'inactive_regions_enabled')

  return not ok or enabled ~= false
end

---Check if regions have changed compared to cached version
---@param filename string Buffer filename
---@param new_regions InactiveRegion[] New regions to compare
---@return boolean
function H.regions_changed(filename, new_regions)
  local cached = InactiveRegions._state.region_cache[filename]

  -- If there are no cached regions or the number of regions differs, considers
  -- the regions as changed.
  if not cached or #cached ~= #new_regions then
    return true
  end

  -- Iterates through each new region and compares it with the corresponding
  -- cached region. Change is detected if any region's start or end
  -- line/character differs.
  for i, region in ipairs(new_regions) do
    local cached_region = cached[i]
    if not cached_region or
       region.start.line ~= cached_region.start.line or
       region.start.character ~= cached_region.start.character or
       region["end"].line ~= cached_region["end"].line or
       region["end"].character ~= cached_region["end"].character then
      return true
    end
  end

  return false
end

---Cancel any pending update for the given buffer
---@param filename string Buffer filename
function H.cancel_pending_update(filename)
  local state = InactiveRegions._state
  local timer = state.debounce_timers[filename]

  if timer then
    vim.fn.timer_stop(timer)
    state.debounce_timers[filename] = nil
  end

  state.pending_updates[filename] = nil

  -- Cancels any active processing coroutine for this buffer. Note that setting
  -- the coroutine reference to nil signals it to terminate if it's designed to
  -- check this.
  state.active_coroutines[filename] = nil
end

---Schedule a debounced update for the buffer
---@param filename string Buffer filename
---@param regions InactiveRegion[] Regions to process
---@param start_time integer Request start time for metrics
function H.schedule_debounced_update(filename, regions, start_time)
  local state = InactiveRegions._state

  state.pending_updates[filename] = true

  -- Starts a timer that, upon expiring, will trigger asynchronous processing of
  -- regions. The delay is configured by `InactiveRegions.config.debounce_ms`.
  local timer = vim.fn.timer_start(InactiveRegions.config.debounce_ms, function()
    state.debounce_timers[filename] = nil
    state.pending_updates[filename] = nil

    H.process_inactive_regions_async(filename, regions, start_time)
  end)

  --Stores the timer ID for potential cancellation.
  state.debounce_timers[filename] = timer
end

-- Async Processing Engine
--

---Process inactive regions asynchronously with proper coroutine management
---@param filename string Buffer filename
---@param regions InactiveRegion[] Regions to process
---@param start_time integer Request start time for metrics
function H.process_inactive_regions_async(filename, regions, start_time)
  local state = InactiveRegions._state

  -- Cancel any existing coroutine for this buffer ibefore starting a new one to
  -- prevents multiple processing tasks from running concurrently for the same
  -- buffer.
  state.active_coroutines[filename] = nil

  local bufnr = vim.fn.bufnr(filename)
  if bufnr == -1 then
    H.log("Buffer no longer exists for %s", filename)
    return
  end

  vim.api.nvim_buf_clear_namespace(bufnr, InactiveRegions.ns, 0, -1)

  if #regions == 0 then
    H.log("No regions to process for %s", filename)
    return
  end

  local co = coroutine.create(function()
    return H.process_regions_coroutine(bufnr, filename, regions, start_time)
  end)

  -- Stores the reference to the new coroutine and resumes it safely.
  state.active_coroutines[filename] = co
  H.resume_coroutine_safely(filename, co)
end

---Safely resume a coroutine with error handling
---@param filename string Buffer filename for context
---@param co thread Coroutine to resume
function H.resume_coroutine_safely(filename, co)
  local success, should_continue = coroutine.resume(co)

  if not success then
    H.log("Error in processing coroutine for %s: %s", filename, should_continue)
    InactiveRegions._state.active_coroutines[filename] = nil
    return
  end

  if coroutine.status(co) == "dead" then
    InactiveRegions._state.active_coroutines[filename] = nil
    H.log("Completed processing for %s", filename)
  elseif should_continue then
    vim.schedule(function()
      -- Before resuming, checks if this coroutine is still the active one for
      -- the buffer. That is, we must handles cases where a new update might
      -- have superseded this processing task.
      if InactiveRegions._state.active_coroutines[filename] == co then
        H.resume_coroutine_safely(filename, co)
      end
    end)
  end
end

---Main coroutine function for processing regions
---@param bufnr integer Buffer number
---@param filename string Buffer filename
---@param regions InactiveRegion[] Regions to process
---@param start_time integer Request start time
---@return boolean Should continue processing
function H.process_regions_coroutine(bufnr, filename, regions, start_time)
  local state = InactiveRegions._state
  local metrics = state.performance_metrics

  local parser = vim.treesitter.get_parser(bufnr)
  if not parser then
    H.log("No treesitter parser available for buffer %d", bufnr)
    return false
  end

  local trees = parser:parse()
  if not trees or #trees == 0 then
    H.log("No parse trees available for buffer %d", bufnr)
    return false
  end

  local query = vim.treesitter.query.get(parser:lang(), "highlights")
  if not query then
    H.log("No highlight query available for language %s", parser:lang())
    return false
  end

  H.ensure_theme_blends_cached()

  local total_highlights = 0
  local nodes_processed = 0

  -- Iterates over each inactive region provided by the LSP.
  for region_idx, region in ipairs(regions) do
    H.log("Processing region %d/%d", region_idx, #regions)

    local highlights_batch = {}

    -- Iterates over Treesitter captures within the current region's line range.
    -- `trees[1]:root()` gets the root node of the primary syntax tree. The
    -- iteration is constrained to lines spanning the inactive region.
    for id, node in query:iter_captures(
      trees[1]:root(),
      bufnr,
      region.start.line,     -- Start line for iteration (0-indexed).
      region["end"].line + 1 -- End line for iteration (exclusive, 0-indexed).
    ) do
      local start_row, start_col, end_row, end_col = node:range()

      -- Filters nodes to include only those strictly within the inactive
      -- region's boundaries.
      if H.is_node_in_region(start_row, end_row, region) then
        -- Gets the name of the capture (e.g., "comment", "keyword").
        local capture_name = query.captures[id]
        if capture_name then
          -- Retrieves or creates a highlight group specific to this capture
          -- name, styled for inactive regions (e.g., with reduced opacity).
          local highlight_group = H.get_or_create_inactive_highlight(capture_name)

          table.insert(highlights_batch, {
            group = highlight_group,
            row = start_row,
            col_start = start_col,
            col_end = end_col,
          })

          total_highlights = total_highlights + 1
        end
      end

      nodes_processed = nodes_processed + 1

      -- Periodically yields control to Neovim main loop to prevents blocking
      -- the UI during intensive processing.
      if nodes_processed % InactiveRegions.config.max_chunk_size == 0 then
        H.apply_highlight_batch(bufnr, highlights_batch)
        highlights_batch = {}

        H.log("Yielding after processing %d nodes", nodes_processed)
        coroutine.yield(true)
      end
    end

    H.apply_highlight_batch(bufnr, highlights_batch)

    coroutine.yield(true)
  end

  metrics.total_regions_processed = metrics.total_regions_processed + #regions
  metrics.total_highlights_applied = metrics.total_highlights_applied + total_highlights

  local end_time = vim.loop.hrtime()
  local processing_time_ms = (end_time - start_time) / 1000000

  if metrics.avg_processing_time_ms == 0 then
    metrics.avg_processing_time_ms = processing_time_ms
  else
    metrics.avg_processing_time_ms = (metrics.avg_processing_time_ms + processing_time_ms) / 2
  end

  metrics.last_update_time_ms = processing_time_ms

  H.log("Completed processing %d regions with %d highlights in %.2fms",
    #regions, total_highlights, processing_time_ms)

  return false
end

---Check if a node is within the bounds of an inactive region
---@param start_row integer Node start row
---@param end_row integer Node end row
---@param region InactiveRegion Region to check against
---@return boolean
function H.is_node_in_region(start_row, end_row, region)
  -- A node is considered within the region if its start row is not before the
  -- region's start and its end row is not after the region's end. Note that we
  -- assumes that regions are defined by line numbers.
  return start_row >= region.start.line and end_row <= region["end"].line
end

---Apply a batch of highlights efficiently
---@param bufnr integer Buffer number
---@param highlights table[] Batch of highlight data
function H.apply_highlight_batch(bufnr, highlights)
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_add_highlight(
      bufnr,
      InactiveRegions.ns,
      hl.group,     -- the name of the highlight group to apply
      hl.row,       -- the 0-indexed line number
      hl.col_start, -- the 0-indexed start column (byte offset)
      hl.col_end    -- the 0-indexed end column (byte offset, exclusive or -1 for end of line)
    )
  end
end

-- Color Management and Caching
--

---Precompute all theme color blends for performance
function H.precompute_theme_blends()
  local state = InactiveRegions._state
  local theme_sig = H.get_theme_signature()

  if state.theme_signature == theme_sig then
    H.log("Theme blends already cached for current theme")
    return
  end

  H.log("Precomputing theme color blends...")

  state.blend_cache = {}
  state.highlight_cache = {}

  local background = H.get_background_color()
  local opacity = InactiveRegions.config.opacity

  local all_highlights = vim.api.nvim_get_hl(0, {})
  local blends_computed = 0

  for name, hl in pairs(all_highlights) do
    if hl.fg then
      local fg_color = type(hl.fg) == "string" and hl.fg or string.format("#%06x", hl.fg)
      local blend_key = fg_color .. "_" .. background .. "_" .. opacity

      if not state.blend_cache[blend_key] then
        state.blend_cache[blend_key] = H.blend_colors_fast(fg_color, background, opacity)
        blends_computed = blends_computed + 1
      end
    end
  end

  state.theme_signature = theme_sig

  H.log("Precomputed %d color blends for theme", blends_computed)
end

---Ensure theme blends are cached for current theme
function H.ensure_theme_blends_cached()
  if not InactiveRegions.config.precompute_theme then
    return
  end

  local current_sig = H.get_theme_signature()
  if InactiveRegions._state.theme_signature ~= current_sig then
    H.precompute_theme_blends()
  end
end

---Get a signature for the current theme for cache invalidation
---@return string
function H.get_theme_signature()
  local normal_hl = vim.api.nvim_get_hl(0, { name = "Normal" })
  local bg = normal_hl.bg or normal_hl.background or 0
  local fg = normal_hl.fg or normal_hl.foreground or 0

  return string.format("%s_%s_%s", vim.g.colors_name or "default", bg, fg)
end

---Get or create an inactive highlight group for a capture
---@param capture_name string Treesitter capture name
---@return string Highlight group name
function H.get_or_create_inactive_highlight(capture_name)
  local state = InactiveRegions._state
  local group_name = "InactiveRegions_" .. capture_name

  if state.highlight_cache[group_name] then
    state.performance_metrics.cache_hits = state.performance_metrics.cache_hits + 1
    return group_name
  end

  state.performance_metrics.cache_misses = state.performance_metrics.cache_misses + 1

  local ts_group = "@" .. capture_name
  local fg_color = H.get_highlight_color(ts_group, "fg")
  local bg_color = H.get_background_color()
  local blended_color = H.get_blended_color(fg_color, bg_color, InactiveRegions.config.opacity)

  vim.api.nvim_set_hl(0, group_name, {
    fg = blended_color,
    default = true
  })

  -- Caches the newly created group name. Note that '1' acts as a presence
  -- marker.
  state.highlight_cache[group_name] = 1

  return group_name
end

---Get blended color, using cache when available
---@param fg string Foreground color
---@param bg string Background color
---@param opacity number Opacity level
---@return string Blended color
function H.get_blended_color(fg, bg, opacity)
  local state = InactiveRegions._state
  local cache_key = fg .. "_" .. bg .. "_" .. opacity

  local cached = state.blend_cache[cache_key]
  if cached then
    state.performance_metrics.cache_hits = state.performance_metrics.cache_hits + 1
    return cached
  end

  state.performance_metrics.cache_misses = state.performance_metrics.cache_misses + 1

  local blended = H.blend_colors_fast(fg, bg, opacity)
  state.blend_cache[cache_key] = blended

  return blended
end

---Fast color blending algorithm
---@param fg string Foreground color (hex)
---@param bg string Background color (hex)
---@param opacity number Opacity (0.0-1.0)
---@return string Blended color (hex)
function H.blend_colors_fast(fg, bg, opacity)
  local fr, fg_g, fb = H.hex_to_rgb(fg)
  local br, bg_g, bb = H.hex_to_rgb(bg)

  local inv_opacity = 1 - opacity

  local r = math.floor(fr * opacity + br * inv_opacity + 0.5)
  local g = math.floor(fg_g * opacity + bg_g * inv_opacity + 0.5)
  local b = math.floor(fb * opacity + bb * inv_opacity + 0.5)

  return string.format("#%02x%02x%02x",
    math.min(255, math.max(0, r)),
    math.min(255, math.max(0, g)),
    math.min(255, math.max(0, b))
  )
end

---Convert hex color to RGB components
---@param hex string Hex color string
---@return integer, integer, integer RGB components
function H.hex_to_rgb(hex)
  hex = hex:gsub("#", "")
  if #hex == 3 then
    -- Expands shorthand hex format (e.g., #RGB to #RRGGBB).
    hex = hex:gsub("(.)", "%1%1")
  end

  return tonumber(hex:sub(1, 2), 16) or 0,
         tonumber(hex:sub(3, 4), 16) or 0,
         tonumber(hex:sub(5, 6), 16) or 0
end

---Get color from highlight group with caching and fallback resolution
---@param group_name string Highlight group name
---@param attr string Attribute ("fg" or "bg")
---@return string Color in hex format
function H.get_highlight_color(group_name, attr)
  local state = InactiveRegions._state
  local cache_key = group_name .. "_" .. attr

  if state.color_cache[cache_key] then
    return state.color_cache[cache_key]
  end

  local color = H.resolve_highlight_color(group_name, attr)
  state.color_cache[cache_key] = color

  return color
end

---Resolve highlight color with fallback chain
---@param group_name string Highlight group name
---@param attr string Attribute to get
---@return string Color in hex format
function H.resolve_highlight_color(group_name, attr)
  local visited = {}

  local function resolve(name)
    if visited[name] then return nil end
    visited[name] = true

    local hl = vim.api.nvim_get_hl(0, { name = name })

    -- Check direct attribute
    local val = hl[attr] or hl[attr == "fg" and "foreground" or "background"]
    if val then
      return type(val) == "string" and val:match("^#") and val
          or string.format("#%06x", val)
    end

    -- If the group is linked to another group (e.g., Comment links to Normal),
    -- recursively resolves the linked group.
    if hl.link then
      return resolve(hl.link)
    end

    return nil
  end

  local color = resolve(group_name)

  -- If the color could not be resolved for the given group (and it's not
  -- 'Normal' itself), attempts to fall back to the 'Normal' group's color for
  -- that attribute.
  if not color and group_name ~= "Normal" then
    color = H.resolve_highlight_color("Normal", attr)
  end

  if not color then
    if attr == "fg" then
      color = vim.o.background == "dark" and "#ffffff" or "#000000"
    else
      color = vim.o.background == "dark" and "#000000" or "#ffffff"
    end
  end

  return color
end

---Get the current background color
---@return string Background color in hex format
function H.get_background_color()
  return H.get_highlight_color("Normal", "bg")
end

-- Event Handling and Autocommands
--

---Set up autocommands for theme changes and cache invalidation
function H.setup_autocommands()
  local augroup = vim.api.nvim_create_augroup("InactiveRegions", { clear = true })

  vim.api.nvim_create_autocmd("ColorScheme", {
    group = augroup,
    callback = function()
      H.log("Colorscheme changed, invalidating caches")
      InactiveRegions.invalidate_caches()
    end
  })

  vim.api.nvim_create_autocmd("BufDelete", {
    group = augroup,
    callback = function(args)
      local filename = vim.api.nvim_buf_get_name(args.buf)
      if filename and filename ~= "" then
        H.cancel_pending_update(filename)
        InactiveRegions._state.region_cache[filename] = nil
        H.log("Cleaned up state for deleted buffer: %s", filename)
      end
    end
  })

  if InactiveRegions.config.live_typing then
    H.setup_live_typing_autocmds(augroup)
  end
end

---Create default highlight groups
function H.create_default_highlights()
  local bg = H.get_background_color()
  local fg = H.get_highlight_color("Normal", "fg")

  -- Defines a base 'InactiveRegions' highlight group. Its foreground color is
  -- the 'Normal' foreground blended with the 'Normal' background using the
  -- configured opacity. The idea is for it to serves as a fallback or generic
  -- inactive style.
  vim.api.nvim_set_hl(0, "InactiveRegions", {
    fg = H.blend_colors_fast(fg, bg, InactiveRegions.config.opacity),
    default = true
  })
end

-- Live Typing Support
--

-- Live Typing Word Boundary Detection
--

---Check if a word boundary character was just typed
---@param bufnr integer Buffer number
---@param row integer Current row (0-based)
---@param col integer Current column (0-based)
---@return boolean True if a boundary character was typed
function H.check_word_boundary_typed(bufnr, row, col)
  -- Word boundary cannot be typed at the very beginning of a line (column 0).
  if col == 0 then return false end

  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
  if not line or col > #line then return false end

  local char = line:sub(col, col)
  local boundary_chars = InactiveRegions.config.boundary_chars

  -- Checks if the typed character is one of the configured boundary characters.
  -- If not, it's not a word boundary event.
  if not boundary_chars:find(char, 1, true) then
    return false
  end

  -- Verifies that there is a non-boundary character (part of a word)
  -- immediately preceding the typed boundary character so that the
  -- boundary character is actually terminating a word.
  local word_start = col - 1
  while word_start >= 1 do
    local prev_char = line:sub(word_start, word_start)
    -- If another boundary character is found before any word character, then
    -- the typed boundary character did not terminate a word (e.g., typing two
    -- spaces).
    if boundary_chars:find(prev_char, 1, true) then
      break
    end
    -- If a word character (`%w`) is found, it confirms a word was just
    -- completed.
    if prev_char:match("%w") then
      return true
    end
    word_start = word_start - 1
  end

  return false
end

---Analyze and update highlight for the completed word before cursor
---@param bufnr integer Buffer number
---@param row integer Current row (0-based)
---@param col integer Current column (0-based)
function H.analyze_and_update_completed_word(bufnr, row, col)
  -- Extracts the word that was just completed, along with its start and end
  -- columns. Note that for extraction, we considers the character at `col` as
  -- the boundary character.
  local word, start_col, end_col = H.extract_word_before_cursor(bufnr, row, col)
  if not word or word == "" then
    H.log("No word found at boundary at (%d,%d)", row, col)
    return
  end

  H.log("Word boundary detected: analyzing word '%s' at (%d,%d-%d)", word, row, start_col, end_col)

  -- Uses Treesitter to determine the correct semantic highlighting for the
  -- extracted word and applies the corresponding inactive-style highlight.
  H.update_word_highlight_with_treesitter(bufnr, row, start_col, end_col, word)
end

---Extract the word that was just completed before the cursor
---@param bufnr integer Buffer number
---@param row integer Current row (0-based)
---@param col integer Current column (0-based)
---@return string|nil, integer|nil, integer|nil Word text, start column, end column
function H.extract_word_before_cursor(bufnr, row, col)
  -- Cannot extract a word if the line is empty or at the very start of the
  -- line.
  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
  if not line or col == 0 then return nil end

  local boundary_chars = InactiveRegions.config.boundary_chars

  -- Determines if the character at the current cursor position (col) is a
  -- boundary character to adjust the search range for the word.
  local just_typed_boundary = false
  if col <= #line then
    local char_at_cursor = line:sub(col, col)
    just_typed_boundary = boundary_chars:find(char_at_cursor, 1, true) ~= nil
  end

  -- Sets the starting point for backward search. If a boundary was just typed,
  -- the word ends at `col - 1`. Otherwise, the word might include `col`.
  local search_start = just_typed_boundary and (col - 1) or col
  local start_col = search_start
  -- Moves backward from `search_start` to find the beginning of the word. The
  -- word starts after the first boundary character encountered, or at the line
  -- start.
  while start_col >= 1 do
    local char = line:sub(start_col, start_col)
    if boundary_chars:find(char, 1, true) then
      start_col = start_col + 1 -- Word starts after this boundary.
      break
    end
    start_col = start_col - 1
  end
  start_col = math.max(start_col, 1) -- start_col must be at least 1 (1-based)

  -- Sets the end point for the word. If a boundary was just typed, the word
  -- ends at `col - 1`. Otherwise, it ends at `col`. Note that it primarily
  -- targets the word *before* a typed boundary.
  local end_col = just_typed_boundary and (col - 1) or col
  -- This loop seems redundant or potentially incorrect if `just_typed_boundary`
  -- is true, as `end_col` would be `col-1`. But if `just_typed_boundary` is false,
  -- it tries to extend `end_col` to the end of the current word segment. For
  -- "word boundary" detection, the word is expected to end *before* the cursor.
  while end_col <= #line do
    local char = line:sub(end_col + 1, end_col + 1)
    if not char or char == "" or boundary_chars:find(char, 1, true) then
      break -- Found end of word or line.
    end
    end_col = end_col + 1
  end

  -- If the calculated start is after the end, no valid word was found.
  if start_col > end_col then return nil end

  local word = line:sub(start_col, end_col)
  -- Returns the extracted word, and its 0-based start and end columns.
  return word, start_col - 1, end_col
end

---Update word highlight using treesitter analysis
---@param bufnr integer Buffer number
---@param row integer Row position (0-based)
---@param start_col integer Start column (0-based)
---@param end_col integer End column (0-based, inclusive)
---@param word string The word text for logging
function H.update_word_highlight_with_treesitter(bufnr, row, start_col, end_col, word)
  -- Cancels any pending timer for immediate character highlighting. That is,
  -- word boundary provides a more definitive highlight, which
  -- supersed temporary ones.
  local state = InactiveRegions._state.live_typing_state
  local existing_timer = state.typing_timers[bufnr]
  if existing_timer then
    vim.fn.timer_stop(existing_timer)
    state.typing_timers[bufnr] = nil
    H.log("Cancelled pending immediate character highlighting timer for word boundary")
  end

  -- Retrieves all existing extmarks (highlights) on the current line within the
  -- module's namespace. It must be done to clear previous temporary highlights
  -- or outdated word highlights that might overlap with the word being
  -- analyzed.
  local all_extmarks = vim.api.nvim_buf_get_extmarks(
    bufnr,
    InactiveRegions.ns,
    {row, 0},
    {row, -1},
    { details = true }
  )

  local cleared_count = 0
  for _, mark in ipairs(all_extmarks) do
    local mark_id, mark_row, mark_col, mark_details = mark[1], mark[2], mark[3], mark[4]

    -- Determine if this extmark overlaps with our word range
    local should_clear = false

    if mark_details and mark_details.hl_group then
      -- Always clear temporary live typing highlights
      if mark_details.hl_group == "InactiveRegions_LiveTyping" then
        should_clear = true
      else
        -- For other (presumably more permanent) highlights, clears only if they
        -- overlap with the current word's range (`start_col` to `end_col + 1`).
        local mark_start = mark_col
        local mark_end = mark_details.end_col or mark_col + 1

        if not (mark_end <= start_col or mark_start >= end_col + 1) then
          should_clear = true
        end
      end
    end

    if should_clear then
      vim.api.nvim_buf_del_extmark(bufnr, InactiveRegions.ns, mark_id)
      cleared_count = cleared_count + 1
    end
  end

  H.log("Cleared %d overlapping extmarks from line %d, preserving others", cleared_count, row)

  local captures = vim.treesitter.get_captures_at_pos(bufnr, row, start_col)
  local highlight_group = "InactiveRegions_LiveTyping" -- default if no specific capture found.
  local capture_name = nil

  -- Iterates through captures to find the most relevant one (often the first or
  -- most specific).
  for _, capture in ipairs(captures) do
    if capture.capture then
      capture_name = capture.capture
      highlight_group = H.get_or_create_inactive_highlight(capture_name)
      break
    end
  end

  if capture_name then
    H.log("Word '%s' identified as @%s, applying highlight %s", word, capture_name, highlight_group)
  else
    -- If no Treesitter capture is found, applies a generic inactive highlight
    -- by blending the 'Normal' foreground with the background at the configured
    -- opacity.
    local bg_color = H.get_background_color()
    local fg_color = H.get_highlight_color("Normal", "fg")
    local blended_color = H.get_blended_color(fg_color, bg_color, InactiveRegions.config.opacity)

    highlight_group = "InactiveRegions_LiveTyping" -- re-affirm or define if not set
    vim.api.nvim_set_hl(0, highlight_group, {
      fg = blended_color,
      default = false -- `default = false` so that it overrides, not links.
    })

    H.log("Word '%s' has no treesitter capture, using default highlight", word)
  end

  H.log("Applying highlight '%s' for word '%s' at (%d,%d-%d)", highlight_group, word, row, start_col, end_col)

  -- Applies the determined highlight group to the word's range. `end_col + 1`
  -- makes the end column exclusive, as expected by `nvim_buf_add_highlight`.
  vim.api.nvim_buf_add_highlight(
    bufnr,
    InactiveRegions.ns,
    highlight_group,
    row,
    start_col,
    end_col + 1 -- `end_col` was inclusive, API needs exclusive or -1.
  )

  -- Resets the temporary highlights state for the buffer, as a definitive
  -- highlight has been applied.
  if state.temp_highlights[bufnr] then
    state.temp_highlights[bufnr] = {}
  end
end

-- Live Typing Basic Functionality
--

---Set up autocommands for live typing blending in inactive regions
---@param augroup integer Autocommand group ID
function H.setup_live_typing_autocmds(augroup)
  vim.api.nvim_create_autocmd("InsertEnter", {
    group = augroup,
    callback = function(args)
      local bufnr = args.buf
      H.update_cursor_inactive_status(bufnr)
    end
  })

  vim.api.nvim_create_autocmd("TextChangedI", {
    group = augroup,
    callback = function(args)
      local bufnr = args.buf
      if H.is_cursor_in_inactive_region(bufnr) then
        H.handle_live_typing(bufnr)
      end
    end
  })

  vim.api.nvim_create_autocmd("CursorMovedI", {
    group = augroup,
    callback = function(args)
      local bufnr = args.buf
      H.update_cursor_inactive_status(bufnr)
    end
  })

  vim.api.nvim_create_autocmd("InsertLeave", {
    group = augroup,
    callback = function(args)
      local bufnr = args.buf
      H.clear_temp_highlights(bufnr)
    end
  })

  -- Listens for a custom "TSReparse" User event, often triggered after
  -- Treesitter finishes re-parsing the buffer (e.g., after a significant change
  -- or file save). If the cursor is in an inactive region, attempts to reapply
  -- more accurate highlights to the typed content, now that Treesitter
  -- information is up-to-date.
  vim.api.nvim_create_autocmd("User", {
    group = augroup,
    pattern = "TSReparse",
    callback = function(args)
      local bufnr = args.buf or vim.api.nvim_get_current_buf()
      if H.is_cursor_in_inactive_region(bufnr) then
        H.reapply_typed_highlights(bufnr)
      end
    end
  })
end

---Update cursor inactive region status for a buffer
---@param bufnr integer Buffer number
function H.update_cursor_inactive_status(bufnr)
  local filename = vim.api.nvim_buf_get_name(bufnr)
  if not filename or filename == "" then return end

  local regions = InactiveRegions._state.region_cache[filename]
  if not regions then return end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local row, col = cursor[1] - 1, cursor[2]
  local in_inactive = false

  for _, region in ipairs(regions) do
    if H.is_position_in_region(row, col, region) then
      in_inactive = true
      break
    end
  end

  InactiveRegions._state.live_typing_state.cursor_in_inactive[bufnr] = in_inactive
  H.log("Cursor in inactive region for buffer %d: %s", bufnr, tostring(in_inactive))
end

---Check if cursor is currently in an inactive region
---@param bufnr integer Buffer number
---@return boolean
function H.is_cursor_in_inactive_region(bufnr)
  return InactiveRegions._state.live_typing_state.cursor_in_inactive[bufnr] or false
end

---Check if a position is within an inactive region
---@param row integer Row position (0-based)
---@param col integer Column position (0-based)
---@param region InactiveRegion Region to check against
---@return boolean
function H.is_position_in_region(row, col, region)
  if row < region.start.line or row > region["end"].line then
    return false
  end

  if row == region.start.line and col < region.start.character then
    return false
  end

  if row == region["end"].line and col > region["end"].character then
    return false
  end

  return true
end

---Handle live typing in inactive regions
---@param bufnr integer Buffer number
function H.handle_live_typing(bufnr)
  local state = InactiveRegions._state.live_typing_state

  -- Cancels any existing debounced timer for live typing highlights so that
  -- rapid typing doesn't queue up multiple highlight applications.
  local existing_timer = state.typing_timers[bufnr]
  if existing_timer then
    vim.fn.timer_stop(existing_timer)
    state.typing_timers[bufnr] = nil
  end

  if InactiveRegions.config.word_boundary_update then
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row, col = cursor[1] - 1, cursor[2] -- Convert to 0-based

    if H.check_word_boundary_typed(bufnr, row, col) then
      H.clear_temp_highlights(bufnr)
      H.analyze_and_update_completed_word(bufnr, row, col)
      return -- Skips the character-by-character highlighting
    end
  end

  if InactiveRegions.config.immediate_char_highlight then
    local timer = vim.fn.timer_start(InactiveRegions.config.typing_debounce_ms, function()
      state.typing_timers[bufnr] = nil
      H.apply_live_typing_highlights(bufnr)
    end)
    state.typing_timers[bufnr] = timer
  end
end

---Apply live typing highlights to newly typed content
---@param bufnr integer Buffer number
function H.apply_live_typing_highlights(bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local row, col = cursor[1] - 1, cursor[2]

  local state = InactiveRegions._state.live_typing_state

  -- Check if we're at a word boundary - if so, don't apply temporary highlights
  -- because word boundary detection should handle it
  if InactiveRegions.config.word_boundary_update and
     H.check_word_boundary_typed(bufnr, row, col) then
    H.log("Skipping live typing highlights - word boundary detected")
    return
  end

  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
  if not line then return end

  local boundary_chars = InactiveRegions.config.boundary_chars

  -- Finds the start of the current word by searching backwards from the cursor
  -- column until a boundary character or the beginning of the line is
  -- encountered.
  local word_start = col
  while word_start > 0 do
    local char = line:sub(word_start, word_start)
    if boundary_chars:find(char, 1, true) then
      word_start = word_start + 1 -- word starts after the boundary.
      break
    end
    word_start = word_start - 1
  end
  word_start = math.max(word_start, 1) -- 1-based index.

  -- Finds the end of the current word by searching forwards from the cursor
  -- column until a boundary character or the end of the line is encountered.
  local word_end = col
  while word_end <= #line do
    local char = line:sub(word_end + 1, word_end + 1)
    if not char or char == "" or boundary_chars:find(char, 1, true) then
      break -- end of word or line.
    end
    word_end = word_end + 1
  end

  -- Converts word start/end to 0-based column indices for highlighting.
  local start_col = word_start - 1
  local end_col = word_end -- `word_end` is inclusive end of word, API needs exclusive.

  -- Updates the last known typed position.
  state.last_typed_pos[bufnr] = { row = row, col = col }

  -- If immediate character highlighting is enabled and a valid word range is
  -- found, applies temporary highlights to this range.
  if InactiveRegions.config.immediate_char_highlight and start_col < end_col then
    H.apply_temp_highlights_to_range(bufnr, row, start_col, row, end_col)

    local word = line:sub(word_start, word_end)
    H.log("Applied live typing highlights to entire word '%s' at buffer %d, range (%d,%d-%d)",
      word, bufnr, row, start_col, end_col)
  end
end

---Apply temporary highlights to a range, blending with inactive region style
---@param bufnr integer Buffer number
---@param start_row integer Start row (0-based)
---@param start_col integer Start column (0-based)
---@param end_row integer End row (0-based)
---@param end_col integer End column (0-based)
function H.apply_temp_highlights_to_range(bufnr, start_row, start_col, end_row, end_col)
  local state = InactiveRegions._state.live_typing_state

  -- Clears any pre-existing temporary highlights for this buffer to prevents
  -- accumulation or overlapping of temporary highlights.
  H.clear_temp_highlights(bufnr)

  -- Determines the color for temporary highlights by blending the 'Normal'
  -- foreground color with the background, using the configured opacity.
  local bg_color = H.get_background_color()
  local fg_color = H.get_highlight_color("Normal", "fg")
  local blended_color = H.get_blended_color(fg_color, bg_color, InactiveRegions.config.opacity)

  -- Defines or redefines the 'InactiveRegions_LiveTyping' highlight group. Note
  -- that we use it specifically for these temporary highlights.
  local temp_group = "InactiveRegions_LiveTyping"
  vim.api.nvim_set_hl(0, temp_group, {
    fg = blended_color,
    default = false -- overrides any underlying text properties.
  })

  local highlight_data = {}
  -- Iterates over the line range to apply highlights. Note that it's structured
  -- to handle multi-line ranges, though typically live typing affects a single
  -- line.
  for line = start_row, end_row do
    local line_start_col = line == start_row and start_col or 0
    local line_end_col = line == end_row and end_col or -1

    -- Before applying a new temporary highlight, clears any existing extmarks
    -- (from this module or others) in the exact target range. This is a more
    -- precise cleanup than the general `clear_temp_highlights` if there's a
    -- need to avoid affecting adjacent, non-temporary highlights. Note that,
    -- `clear_temp_highlights` may have already ran, so this might be redundant
    -- or target different types of overlaps, but it's okay to ignore.
    local existing_marks = vim.api.nvim_buf_get_extmarks(
      bufnr,
      InactiveRegions.ns,
      {line, line_start_col},
      {line, line_end_col == -1 and -1 or line_end_col},
      { details = false }
    )

    for _, mark in ipairs(existing_marks) do
      local mark_id = mark[1]
      vim.api.nvim_buf_del_extmark(bufnr, InactiveRegions.ns, mark_id)
    end

    local hl_id = vim.api.nvim_buf_add_highlight(
      bufnr,
      InactiveRegions.ns,
      temp_group,
      line,
      line_start_col,
      line_end_col
    )

    table.insert(highlight_data, {
      id = hl_id,
      row = line,
      start_col = line_start_col,
      end_col = line_end_col == -1 and math.huge or line_end_col,
      group = temp_group
    })

    H.log("Applied temp highlight to line %d, cols %d-%s",
      line, line_start_col, line_end_col == -1 and "end" or tostring(line_end_col))
  end

  state.temp_highlights[bufnr] = highlight_data
end

---Clear temporary highlights for a buffer
---@param bufnr integer Buffer number
function H.clear_temp_highlights(bufnr)
  local state = InactiveRegions._state.live_typing_state
  local highlight_data = state.temp_highlights[bufnr]

  if highlight_data then
    -- Attempts to remove each tracked temporary highlight by its stored ID.
    -- `pcall` is used because an extmark might have been removed by other
    -- means.
    for _, hl in ipairs(highlight_data) do
      if hl.id then
        pcall(vim.api.nvim_buf_del_extmark, bufnr, InactiveRegions.ns, hl.id)
      end
    end

    state.temp_highlights[bufnr] = {}
    H.log("Cleared %d temp highlights for buffer %d", #highlight_data, bufnr)
  end

  -- As a fallback or broader cleanup, iterates through all extmarks in
  -- the module's namespace and removes any that use the
  -- 'InactiveRegions_LiveTyping' group. This catches any highlights
  -- that might not have been tracked by ID.
  local all_marks = vim.api.nvim_buf_get_extmarks(
    bufnr,
    InactiveRegions.ns,
    0,
    -1,
    { details = true }
  )

  local cleared_count = 0
  for _, mark in ipairs(all_marks) do
    local mark_id, _, _, mark_details = mark[1], mark[2], mark[3], mark[4]
    if mark_details and mark_details.hl_group == "InactiveRegions_LiveTyping" then
      vim.api.nvim_buf_del_extmark(bufnr, InactiveRegions.ns, mark_id)
      cleared_count = cleared_count + 1
    end
  end

  if cleared_count > 0 then
    H.log("Cleared %d additional InactiveRegions_LiveTyping highlights", cleared_count)
  end

  state.last_typed_pos[bufnr] = nil
end

---Request a highlight refresh for a buffer (for when treesitter reparses)
---@param bufnr integer Buffer number
function H.reapply_typed_highlights(bufnr)
  local filename = vim.api.nvim_buf_get_name(bufnr)
  if not filename or filename == "" then return end

  if not H.is_cursor_in_inactive_region(bufnr) then return end

  local cursor = vim.api.nvim_win_get_cursor(0)
  local row, col = cursor[1] - 1, cursor[2]

  -- Attempts to get Treesitter captures at the current cursor position. After a
  -- reparse, Treesitter should have more accurate information.
  local captures = vim.treesitter.get_captures_at_pos(bufnr, row, col)
  if #captures > 0 then
    -- If captures are found, it implies Treesitter has identified syntax
    -- elements here.
    for _, capture in ipairs(captures) do
      if capture.capture then
        -- Gets or creates the appropriate inactive-style highlight group for
        -- the capture.
        local highlight_group = H.get_or_create_inactive_highlight(capture.capture)

        -- Applies this highlight to a small range around the cursor. This is
        -- intended to "correct" the temporary highlight with a Treesitter-aware
        -- one. The range `col-1` to `col+1` might need adjustment depending on
        -- typical node sizes.
        vim.api.nvim_buf_add_highlight(
          bufnr,
          InactiveRegions.ns,
          highlight_group,
          row,
          math.max(0, col - 1), -- Start column, ensuring non-negative.
          col + 1               -- End column (exclusive).
        )
      end
    end
    H.log("Reapplied proper highlights for typed content in buffer %d", bufnr)
  end
end

---Request a highlight refresh for a buffer
---@param bufnr integer Buffer number
function H.request_highlight_refresh(bufnr)
  local filename = vim.api.nvim_buf_get_name(bufnr)
  if not filename or filename == "" then return end

  -- Trigger a refresh by simulating a small change in regions. The intent
  -- is to trigger a re-evaluation of highlights, possibly by forcing a
  -- Treesitter reparse of the affected area.
  vim.schedule(function()
    if vim.api.nvim_buf_is_valid(bufnr) then
      local parser = vim.treesitter.get_parser(bufnr)
      if parser then
        -- Forces the parser to re-parse the buffer. `true` might indicate an
        -- incremental parse or a full reparse depending on context. Ideally, it
        -- should lead to new highlight information if Treesitter emits events
        -- or if subsequent logic queries Treesitter.
        --
        -- Note: Simply reparsing might not be enough. The system relies on LSP
        -- sending new `inactiveRegions` or an event like `TSReparse` (if
        -- configured) to trigger the main highlighting logic. This function
        -- alone might not directly cause `H.process_inactive_regions_async` to
        -- run without further integration.
        parser:parse(true)
      end
    end
  end)
end

-- Debug and Development Tools
--

---Set up debug commands for development and troubleshooting
function H.setup_debug_commands()
  vim.api.nvim_create_user_command("InactiveRegionsMetrics", function()
    local metrics = InactiveRegions.get_metrics()
    print("=== Inactive Regions Performance Metrics ===")
    print(string.format("Total regions processed: %d", metrics.total_regions_processed))
    print(string.format("Total highlights applied: %d", metrics.total_highlights_applied))
    print(string.format("Cache hits: %d", metrics.cache_hits))
    print(string.format("Cache misses: %d", metrics.cache_misses))
    print(string.format("Cache hit ratio: %.2f%%",
      metrics.cache_hits / math.max(1, metrics.cache_hits + metrics.cache_misses) * 100))
    print(string.format("Average processing time: %.2fms", metrics.avg_processing_time_ms))
    print(string.format("Last update time: %.2fms", metrics.last_update_time_ms))
  end, {})

  vim.api.nvim_create_user_command("InactiveRegionsCacheInfo", function()
    local state = InactiveRegions._state
    print("=== Inactive Regions Cache Information ===")
    print(string.format("Color cache entries: %d", vim.tbl_count(state.color_cache)))
    print(string.format("Blend cache entries: %d", vim.tbl_count(state.blend_cache)))
    print(string.format("Highlight cache entries: %d", vim.tbl_count(state.highlight_cache)))
    print(string.format("Cached buffers: %d", vim.tbl_count(state.region_cache)))
    print(string.format("Active coroutines: %d", vim.tbl_count(state.active_coroutines)))
    print(string.format("Theme signature: %s", state.theme_signature))
  end, {})

  vim.api.nvim_create_user_command("InactiveRegionsRecompute", function()
    InactiveRegions.invalidate_caches()
    print("Forced cache invalidation and recomputation")
  end, {})

  vim.api.nvim_create_user_command("InactiveRegionsDebugNode", function()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row, col = cursor[1] - 1, cursor[2]

    local captures = vim.treesitter.get_captures_at_pos(bufnr, row, col)
    if #captures == 0 then
      print("No treesitter captures at cursor position")
      return
    end

    print("=== Node Debug Information ===")
    for _, capture in ipairs(captures) do
      if capture.capture then
        local ts_group = "@" .. capture.capture
        local fg_color = H.get_highlight_color(ts_group, "fg")
        local bg_color = H.get_background_color()
        local blended = H.get_blended_color(fg_color, bg_color, InactiveRegions.config.opacity)

        print(string.format("Capture: %s", capture.capture))
        print(string.format("  TS Group: %s", ts_group))
        print(string.format("  FG Color: %s", fg_color))
        print(string.format("  BG Color: %s", bg_color))
        print(string.format("  Blended: %s", blended))
      end
    end
  end, {})

  vim.api.nvim_create_user_command("InactiveRegionsDebugWord", function()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row, col = cursor[1] - 1, cursor[2]

    print("=== Word Boundary Debug Information ===")
    print(string.format("Cursor position: (%d, %d)", row, col))

    if col > 0 then
      local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
      if line then
        local char = line:sub(col, col)
        local is_boundary = InactiveRegions.config.boundary_chars:find(char, 1, true) ~= nil
        print(string.format("Character at cursor: '%s'", char))
        print(string.format("Is word boundary: %s", tostring(is_boundary)))

        local word, start_col, end_col = H.extract_word_before_cursor(bufnr, row, col)
        if word then
          print(string.format("Word before cursor: '%s' at (%d-%d)", word, start_col, end_col))

          local captures = vim.treesitter.get_captures_at_pos(bufnr, row, start_col)
          if #captures > 0 then
            for _, capture in ipairs(captures) do
              if capture.capture then
                print(string.format("Word treesitter capture: @%s", capture.capture))
                break
              end
            end
          else
            print("No treesitter captures for word")
          end
        else
          print("No word found before cursor")
        end
      end
    else
      print("At beginning of line")
    end

    print(string.format("Word boundary update enabled: %s", tostring(InactiveRegions.config.word_boundary_update)))
    print(string.format("Immediate char highlight enabled: %s", tostring(InactiveRegions.config.immediate_char_highlight)))
    print(string.format("Boundary chars: '%s'", InactiveRegions.config.boundary_chars))
  end, {})
end

---Debug logging function
---@param format string Format string
---@param ... any Arguments for format string
function H.log(format, ...)
  if InactiveRegions.config.debug then
    local timestamp = os.date("%H:%M:%S")
    print(string.format("[%s] InactiveRegions: " .. format, timestamp, ...))
  end
end

-- Module Export
--

InactiveRegions.config = default_config

return InactiveRegions
