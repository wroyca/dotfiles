local M = {}

--- Filetypes where clangd transformations should be applied
--- @type table<string, true>
local RELEVANT_FILETYPES = {
  c = true,
  cpp = true,
}

--- Memoized buffer filetype check cache
--- @type table<integer, boolean>
local filetype_cache = setmetatable({}, {
  __index = function(t, bufnr)
    local ft = vim.bo[bufnr].filetype
    local result = RELEVANT_FILETYPES[ft] == true
    rawset(t, bufnr, result)
    return result
  end,
})

--- Cache for project root directories
--- @type table<integer, string|false>
local root_dir_cache = {}

--- Cache for project names derived from root directories
--- @type table<string, string>
local project_name_cache = {}

--- Strips a leading whitespace character from a string field.
---
--- Clangd inserts a space character when `completeIncludeInsertion` is disabled,
--- causing completion labels to appear misaligned. This function removes exactly
--- one leading space or tab character.
---
--- @param field string|nil The string field to normalize
--- @return string|nil The field with leading whitespace removed
--- @private
local function strip_leading_whitespace(field)
  if not field or #field == 0 then
    return field
  end

  local first_byte = string.byte(field, 1)
  if first_byte == 32 or first_byte == 9 then -- SPACE or TAB
    return string.sub(field, 2)
  end

  return field
end

--- Retrieves the project root directory for a buffer.
---
--- Uses a two-tier caching strategy:
--- 1. Check cache for previously resolved root
--- 2. Query clangd LSP client for root_dir
---
--- @param bufnr integer Buffer number
--- @return string|nil Root directory path
--- @private
local function get_root_dir(bufnr)
  local cached = root_dir_cache[bufnr]
  if cached ~= nil then
    return cached ~= false and cached or nil
  end

  local clients = vim.lsp.get_clients({ bufnr = bufnr, name = "clangd" })
  local root_dir = clients and clients[1] and clients[1].config.root_dir

  -- Cache result (use false sentinel for nil)
  root_dir_cache[bufnr] = root_dir or false
  return root_dir
end

--- Retrieves the project name from a root directory.
---
--- Extracts the final path component (e.g., "iw4x" from "/home/user/projects/iw4x").
---
--- @param root_dir string Root directory path
--- @return string|nil Project name
--- @private
local function get_project_name(root_dir)
  local cached = project_name_cache[root_dir]
  if cached then
    return cached
  end

  local name = vim.fn.fnamemodify(root_dir, ":t")
  project_name_cache[root_dir] = name
  return name
end

--- Prioritizes completion items that appear to be project headers.
---
--- Boosts the score of items whose paths contain the project name,
--- with higher scores for matches at the beginning of the path.
---
--- Scoring:
---   - Matches at path start (^project/): +1000
---   - Matches anywhere in path (/project/): +500
---
--- @param bufnr integer Buffer number
--- @param items blink.cmp.CompletionItem[] Completion items to prioritize
--- @return blink.cmp.CompletionItem[] Sorted items with boosted scores
--- @private
local function prioritize_project_headers(bufnr, items)
  local root_dir = get_root_dir(bufnr)
  if not root_dir then
    return items
  end

  local project_name = get_project_name(root_dir)
  if not project_name then
    return items
  end

  -- Build scoring patterns based on project name
  local patterns = {}
  if string.find(project_name, "^lib") then
    local name_without_lib = string.sub(project_name, 4)
    patterns = {
      { pattern = "^" .. vim.pesc(project_name) .. "/", score = 1000 },
      { pattern = "/" .. vim.pesc(project_name) .. "/", score = 500 },
    }
    if #name_without_lib > 0 then
      patterns[#patterns + 1] = { pattern = "^" .. vim.pesc(name_without_lib) .. "/", score = 1000 }
      patterns[#patterns + 1] = { pattern = "/" .. vim.pesc(name_without_lib) .. "/", score = 500 }
    end
  else
    local lib_project_name = "lib" .. project_name
    patterns = {
      { pattern = "^" .. vim.pesc(project_name) .. "/", score = 1000 },
      { pattern = "^" .. vim.pesc(lib_project_name) .. "/", score = 1000 },
      { pattern = "/" .. vim.pesc(project_name) .. "/", score = 500 },
      { pattern = "/" .. vim.pesc(lib_project_name) .. "/", score = 500 },
    }
  end

  -- Apply score boosts
  for i = 1, #items do
    local item = items[i]
    local text = item.label or item.insertText or ""

    for j = 1, #patterns do
      local boost = patterns[j]
      if string.find(text, boost.pattern) then
        item.score_offset = (item.score_offset or 0) + boost.score
        break
      end
    end
  end

  -- Sort by score (descending), then alphabetically
  table.sort(items, function(a, b)
    local score_a = a.score_offset or 0
    local score_b = b.score_offset or 0
    if score_a ~= score_b then
      return score_a > score_b
    end
    return (a.label or "") < (b.label or "")
  end)

  return items
end

--- Transforms completion items for clangd-specific optimizations.
---
--- Context-aware transformations:
---   1. Include directive context (`#include <...>`):
---      - Injects project header directories
---      - Prioritizes project headers
---      - Strips whitespace from labels
---
---   2. General completion context:
---      - Strips leading whitespace from all fields
---
--- @param ctx blink.cmp.Context Completion context from blink.cmp
--- @param items blink.cmp.CompletionItem[] Completion items from LSP
--- @return blink.cmp.CompletionItem[] Transformed completion items
function M.transform_items(ctx, items)
  local bufnr = ctx.bufnr

  -- Early exit for non-C/C++ buffers
  if not bufnr or not filetype_cache[bufnr] then
    return items
  end

  -- Detect include directive context
  local in_include = false
  if ctx.line and string.find(ctx.line, "#include%s*<") then
    local cursor_col = ctx.cursor and ctx.cursor[2] or 0
    local before_cursor = string.sub(ctx.line, 1, cursor_col)

    if string.find(before_cursor, "<") and not string.find(before_cursor, ">") then
      in_include = true

      -- NOTE:
      --
      -- Clangd's behaviour in the <> include context remains deliberately broad.
      -- It surfaces a large, unspecialized candidate set, which has the practical
      -- effect of burying project-local headers until the user provides a
      -- sufficiently narrow prefix. The first completion request is therefore
      -- more noise than signal.
      --
      -- Increasing clangd's result window does help to a point. With a
      -- higher --limit-results value (we settled on 7000), clangd avoids early
      -- truncation and retains more of its internal ranking structure. This
      -- does not change the underlying heuristics, but it improves the odds of
      -- project headers appearing without pathological latency.
    end
  end

  -- Strip leading whitespace from all items
  for i = 1, #items do
    local item = items[i]
    item.label = strip_leading_whitespace(item.label)
    item.insertText = strip_leading_whitespace(item.insertText)
    item.filterText = strip_leading_whitespace(item.filterText)

    local textEdit = item.textEdit
    if textEdit and textEdit.newText then
      textEdit.newText = strip_leading_whitespace(textEdit.newText)
    end
  end

  -- Prioritize project headers in include context
  if in_include then
    return prioritize_project_headers(bufnr, items)
  end

  return items
end

return M
