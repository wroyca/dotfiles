-- Module definition
local InactiveRegions = {}
local H = {}

--- Module setup
---
---@param config table|nil Module config table. See |InactiveRegions.config|.
---
---@usage `require('inactive_regions').setup({})` (replace `{}` with your `config` table)
InactiveRegions.setup = function(config)
  -- Export module
  _G.InactiveRegions = InactiveRegions

  -- Setup config
  config = H.setup_config(config)
  InactiveRegions.config = config

  -- Apply config
  H.apply_config(config)

  -- Create highlighting
  H.create_default_hl()

  -- Set up autocommands for cache invalidation
  H.setup_cache_clearing()

  -- Initialize caches
  H.color_cache = {}
  H.inactive_highlight_cache = {}
  H.last_background = nil
  H.previous_regions = {}

  -- Apply to registered LSPs
  H.setup_lsp_handlers()
end

--- Module config
---
--- Default values:
---@eval return MiniDoc.afterlines_to_code(MiniDoc.current.eval_section)
InactiveRegions.config = {
  -- Opacity level for inactive regions (0.0 to 1.0)
  opacity = 0.5,

  -- Namespace for highlights
  namespace = vim.api.nvim_create_namespace("inactive_regions"),

  -- Debug mode with logging and commands
  debug = false,
}

InactiveRegions.ns = vim.api.nvim_create_namespace("inactive_regions")

-- Enable inactive regions for specific buffers
---@param bufnr number|nil Buffer number (default: current buffer)
InactiveRegions.enable = function(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_var(bufnr, 'inactive_regions_enabled', true)
end

-- Disable inactive regions for specific buffers
---@param bufnr number|nil Buffer number (default: current buffer)
InactiveRegions.disable = function(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(bufnr, InactiveRegions.ns, 0, -1)
  vim.api.nvim_buf_set_var(bufnr, 'inactive_regions_enabled', false)
end

-- Function to Toggle debugging
InactiveRegions.toggle_debug = function()
  InactiveRegions.config.debug = not InactiveRegions.config.debug
  if InactiveRegions.config.debug then H.setup_debug_commands() end
end

-- Function to clear all caches (useful for troubleshooting)
InactiveRegions.clear_cache = function()
  H.color_cache = {}
  H.inactive_highlight_cache = {}
  H.last_background = nil
  H.previous_regions = {}
  print("Cleared all InactiveRegions caches")
end

-- Helper functionality =======================================================

-- Internal state
H.color_cache = {}
H.inactive_highlight_cache = {}
H.last_background = nil
H.previous_regions = {}

-- Set up and validate config
H.setup_config = function(config)
  config = vim.tbl_deep_extend("force", vim.deepcopy(InactiveRegions.config), config or {})

  if config.opacity < 0 or config.opacity > 1 then
    vim.notify("InactiveRegions: opacity must be between 0.0 and 1.0", vim.log.levels.WARN)
    config.opacity = 0.5
  end

  return config
end

-- Apply config
H.apply_config = function(config)
  InactiveRegions.ns = vim.api.nvim_create_namespace("inactive_regions")
  if config.debug then
    H.setup_debug_commands()
  end
end

-- Set up autocommands for cache invalidation
H.setup_cache_clearing = function()
  vim.api.nvim_create_autocmd("ColorScheme", {
    callback = function()
      H.color_cache = {}
      H.inactive_highlight_cache = {}
      H.last_background = nil
      H.log("Cleared highlight caches due to colorscheme change")
    end
  })
end

-- Register LSP handlers for inactive regions
H.setup_lsp_handlers = function()
  vim.lsp.handlers["textDocument/inactiveRegions"] = H.handle_inactive_regions
end

-- Create default highlight groups
H.create_default_hl = function()
  local bg = H.get_background_color()
  local fg = H.get_foreground_color()
  vim.api.nvim_set_hl(0, "InactiveRegions", {
    fg = H.blend_colors(fg, bg, InactiveRegions.config.opacity),
    default = true
  })
end

-- Debug logging
H.log = function(...)
  if InactiveRegions.config.debug then
    print(string.format(...))
  end
end

-- Set up debug commands
H.setup_debug_commands = function()
  -- Command to debug node under cursor
  vim.api.nvim_create_user_command("InactiveRegionsDebugNode", function()
    local bufnr = vim.api.nvim_get_current_buf()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row, col = cursor[1] - 1, cursor[2]

    local captures = vim.treesitter.get_captures_at_pos(bufnr, row, col)
    if #captures == 0 then
      print("No treesitter captures at cursor position")
      return
    end

    print("=== Node under cursor ===")
    local bg = H.get_background_color()
    print("Background color: " .. bg)

    for _, capture in ipairs(captures) do
      if capture.capture then
        print("\nCapture: " .. capture.capture)
        local ts_group = "@" .. capture.capture
        print("TS Group: " .. ts_group)

        H.dump_highlight(ts_group)

        local color = H.get_highlight_color(ts_group, "fg")
        print("Resolved color: " .. color)

        local blended = H.blend_colors(color, bg, InactiveRegions.config.opacity)
        print("Blended color: " .. blended)
      end
    end
  end, {})

  -- Command to clear caches
  vim.api.nvim_create_user_command("InactiveRegionsClearCache", function()
    InactiveRegions.clear_cache()
  end, {})

  -- Command to toggle debug mode
  vim.api.nvim_create_user_command("InactiveRegionsToggleDebug", function()
    InactiveRegions.toggle_debug()
  end, {})
end

-- Dump highlight details for debugging
H.dump_highlight = function(name)
  if not InactiveRegions.config.debug then return end

  local hl = vim.api.nvim_get_hl(0, { name = name })
  H.log("Highlight %s: %s", name, vim.inspect(hl))

  if hl.link then
    H.log("  Links to: %s", hl.link)
    H.dump_highlight(hl.link)
  end
end

-- Color utility functions ====================================================

-- Convert hex color to RGB values
H.hex_to_rgb = function(hex)
  hex = hex:gsub("#", "")
  return tonumber(hex:sub(1, 2), 16) or 0,
         tonumber(hex:sub(3, 4), 16) or 0,
         tonumber(hex:sub(5, 6), 16) or 0
end

-- Convert RGB values to hex color
H.rgb_to_hex = function(r, g, b)
  return string.format("#%02x%02x%02x", r, g, b)
end

-- Blend two colors with specified opacity
H.blend_colors = function(fg, bg, opacity)
  local cache_key = fg .. "_" .. bg .. "_" .. opacity
  if H.color_cache[cache_key] then
    return H.color_cache[cache_key]
  end

  local fr, fg_, fb = H.hex_to_rgb(fg)
  local br, bg_, bb = H.hex_to_rgb(bg)
  local result = H.rgb_to_hex(
    math.floor(fr * opacity + br * (1 - opacity)),
    math.floor(fg_ * opacity + bg_ * (1 - opacity)),
    math.floor(fb * opacity + bb * (1 - opacity))
  )

  H.color_cache[cache_key] = result
  return result
end

-- Get color from highlight group, following the link chain
H.get_highlight_color = function(name, attr)
  local cache_key = name .. "_" .. attr
  if H.color_cache[cache_key] then
    return H.color_cache[cache_key]
  end

  H.log("Looking for %s color in %s", attr, name)

  -- Track visited highlights to prevent infinite loops
  local visited = {}

  -- Recursive function to follow links
  local function resolve(group)
    if not group or visited[group] then return nil end
    visited[group] = true

    H.log("  Checking %s", group)

    -- Special case for @spell - use Comment highlight
    if group == "@spell" and not visited["Comment"] then
      H.log("  Special case for @spell: using Comment")
      return resolve("Comment")
    end

    local hl = vim.api.nvim_get_hl(0, { name = group })

    -- Check for direct attribute
    local val = hl[attr] or hl[attr == "fg" and "foreground" or "background"]
    if val then
      H.log("  Found color: %s", val)
      return type(val) == "string" and val:match("^#") and val
          or string.format("#%06x", val)
    end

    -- Actual color might be several links deep
    if hl.link then
      H.log("  Following link: %s -> %s", group, hl.link)
      return resolve(hl.link)
    end

    -- For treesitter groups, try fallbacks
    if group:match("^@") then
      local buffer = vim.api.nvim_get_current_buf()
      local lang = vim.bo[buffer].filetype

      -- Try removing language suffix
      if group:match("%.[^.]+$") then
        local base = group:gsub("%.[^.]+$", "")
        H.log("  Trying base group: %s", base)
        local result = resolve(base)
        if result then return result end
      end

      -- Try standard highlight name (e.g., Function for @function)
      local base_name = group:gsub("^@", ""):gsub("%..*$", "")
      local std_name = base_name:sub(1,1):upper() .. base_name:sub(2)
      H.log("  Trying standard name: %s", std_name)
      local result = resolve(std_name)
      if result then return result end
    end

    return nil
  end

  -- Start resolving from the requested group
  local color = resolve(name)

  -- Fall back to Normal colors if nothing found
  if not color and name ~= "Normal" then
    H.log("  Falling back to Normal")
    color = H.get_highlight_color("Normal", attr)
  end

  -- Final fallback based on background
  if not color then
    color = attr == "fg" and "#ffffff" or "#000000"
    if vim.o.background == "light" and attr == "bg" then
      color = "#ffffff"
    end
    H.log("  Using default color: %s", color)
  end

  H.color_cache[cache_key] = color
  return color
end

-- Get background color
H.get_background_color = function()
  return H.get_highlight_color("Normal", "bg")
end

-- Get foreground color
H.get_foreground_color = function()
  return H.get_highlight_color("Normal", "fg")
end

-- Ensure an inactive highlight group exists
H.ensure_inactive_group = function(capture_name, background_color)
  local inactive_group = "InactiveRegions_" .. capture_name

  -- Check if we already created this highlight group with the current background
  if H.inactive_highlight_cache[inactive_group] == background_color then
    return inactive_group
  end

  -- Get the color for this capture and blend it with the background
  local ts_group = "@" .. capture_name
  local fg_color = H.get_highlight_color(ts_group, "fg")
  local blended = H.blend_colors(fg_color, background_color, InactiveRegions.config.opacity)

  -- Create the highlight group
  vim.api.nvim_set_hl(0, inactive_group, {
    fg = blended,
    default = true
  })

  H.inactive_highlight_cache[inactive_group] = background_color
  return inactive_group
end

-- Handler for inactive regions LSP notification
H.handle_inactive_regions = function(_, message, _, _)
  local uri = message.textDocument.uri
  local filename = vim.uri_to_fname(uri)
  local regions = message.regions

  if #regions == 0 then
    -- If there are no regions but we had regions before, clear them
    if H.previous_regions[filename] and #H.previous_regions[filename] > 0 then
      if vim.fn.bufexists(filename) ~= 0 then
        local buffer = vim.fn.bufadd(filename)
        vim.api.nvim_buf_clear_namespace(buffer, InactiveRegions.ns, 0, -1)
      end
      H.previous_regions[filename] = {}
    end
    return
  end

  if vim.fn.bufexists(filename) == 0 then
    return
  end

  -- Add buffer if needed
  local buffer = vim.fn.bufadd(filename)

  -- Check if buffer has InactiveRegions disabled
  local is_enabled = true
  local ok, val = pcall(vim.api.nvim_buf_get_var, buffer, 'inactive_regions_enabled')
  if ok and val == false then
    is_enabled = false
  end

  if not is_enabled then
    H.log("InactiveRegions disabled for buffer %d, skipping", buffer)
    return
  end

  -- Check if we need to process these regions
  local regions_changed = not H.previous_regions[filename] or #regions ~= #H.previous_regions[filename]

  if not regions_changed then
    -- Check each region to see if any changed
    for i, region in ipairs(regions) do
      local prev_region = H.previous_regions[filename][i]
      if region.start.line ~= prev_region.start.line or
         region["end"].line ~= prev_region["end"].line then
        regions_changed = true
        break
      end
    end
  end

  -- If regions haven't changed, we can skip processing
  if not regions_changed then
    H.log("Inactive regions unchanged, skipping processing")
    return
  end

  -- Save the new regions for future comparison
  H.previous_regions[filename] = vim.deepcopy(regions)

  -- Clear existing highlights only now that we've determined changes are needed
  vim.api.nvim_buf_clear_namespace(buffer, InactiveRegions.ns, 0, -1)

  -- Get the background color for blending
  local background_color = H.get_background_color()
  H.log("Background color: %s", background_color)

  -- Check if background color changed, invalidate cache if needed
  if H.last_background ~= background_color then
    H.inactive_highlight_cache = {} -- Clear inactive highlight cache
    H.last_background = background_color
    H.log("Background color changed, cleared highlight cache")
  end

  -- Start coroutine for processing inactive regions
  local co = coroutine.create(function()
    -- Process each inactive region
    for region_idx, region in ipairs(regions) do
      local start_line = region.start.line
      local end_line = region["end"].line

      -- Get parser and tree for syntax highlighting
      local parser = vim.treesitter.get_parser(buffer)
      if not parser then coroutine.yield(true) return end

      local tree = parser:parse()[1]
      if not tree then coroutine.yield(true) return end

      local query = vim.treesitter.query.get(parser:lang(), "highlights")
      if not query then coroutine.yield(true) return end

      -- Process nodes in smaller chunks
      local captures = {}
      local nodes_processed = 0

      -- Collect all captures first
      for id, node in query:iter_captures(tree:root(), buffer, start_line, end_line + 1) do
        local start_row, start_col, _, end_col = node:range()

        -- Only collect nodes within our inactive region
        if start_row >= start_line and start_row <= end_line then
          local node_captures = vim.treesitter.get_captures_at_pos(buffer, start_row, start_col)

          for _, capture in ipairs(node_captures) do
            if capture.capture then
              table.insert(captures, {
                capture_name = capture.capture,
                start_row = start_row,
                start_col = start_col,
                end_col = end_col
              })
            end
          end
        end

        -- Yield every 500 nodes to prevent blocking
        nodes_processed = nodes_processed + 1
        if nodes_processed % 500 == 0 then
          H.log("Yielding after processing %d nodes in region %d", nodes_processed, region_idx)
          coroutine.yield()
        end
      end

      -- Now process the captures in chunks
      H.log("Processing %d captures for region %d", #captures, region_idx)
      for i, capture_data in ipairs(captures) do
        -- Get or create highlight group with caching
        local inactive_group = H.ensure_inactive_group(
          capture_data.capture_name,
          background_color
        )

        -- Apply highlight to this node
        vim.api.nvim_buf_add_highlight(
          buffer,
          InactiveRegions.ns,
          inactive_group,
          capture_data.start_row,
          capture_data.start_col,
          capture_data.end_col
        )

        -- Yield every 200 highlights to prevent blocking
        if i % 200 == 0 then
          H.log("Yielding after applying %d highlights in region %d", i, region_idx)
          coroutine.yield()
        end
      end

      -- Yield after each region
      coroutine.yield()
    end
  end)

  -- Run the coroutine
  local function resume_coroutine()
    local success, result = coroutine.resume(co)

    if not success then
      -- Log error but don't crash
      H.log("Error in inactive regions coroutine: %s", result)
      return
    end

    if coroutine.status(co) ~= "dead" then
      -- Schedule the next step of the coroutine
      vim.schedule(resume_coroutine)
    else
      H.log("Inactive regions processing complete")
    end
  end

  -- Start coroutine processing
  vim.schedule(resume_coroutine)
end

return InactiveRegions
