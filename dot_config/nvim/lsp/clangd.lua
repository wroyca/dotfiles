local inactive_regions = vim.api.nvim_create_namespace ("inactive_regions")

---@param hex string Hex color (with/without #)
---@return number,number,number RGB values
local function hex_to_rgb (hex)
  hex = hex:gsub ("#", "")
  return tonumber (hex:sub (1, 2), 16) or 0, tonumber (hex:sub (3, 4), 16) or 0, tonumber (hex:sub (5, 6), 16) or 0
end

---@param r number Red (0-255)
---@param g number Green (0-255)
---@param b number Blue (0-255)
---@return string Hex color
local function rgb_to_hex (r, g, b)
  return string.format ("#%02x%02x%02x", r, g, b)
end

---@param grp string Highlight group name
---@param typ? "foreground"|"background" Color type
---@return string Hex color
local function get_highlight_color (grp, typ)
  typ = typ or "foreground"
  local hl = vim.api.nvim_get_hl (0, { name = grp })
  if hl.link then hl = vim.api.nvim_get_hl (0, { name = hl.link }) end
  local val = typ == "foreground" and (hl.fg or hl.foreground) or (hl.bg or hl.background)
  if not val then return typ == "foreground" and "#ffffff" or "#000000" end
  if type (val) == "string" and val:match ("^#") then return val end
  return string.format ("#%06x", val)
end

---@param fg string Foreground color
---@param bg string Background color
---@param op number Opacity (0-1)
---@return string Blended color
local function blend_colors (fg, bg, op)
  local fr, fg, fb = hex_to_rgb (fg)
  local br, bg, bb = hex_to_rgb (bg)
  return rgb_to_hex (
    math.floor (fr * op + br * (1 - op)),
    math.floor (fg * op + bg * (1 - op)),
    math.floor (fb * op + bb * (1 - op))
  )
end

---@return string Background color
local function get_background_color ()
  local hl = vim.api.nvim_get_hl (0, { name = "Normal" })
  if hl.bg or hl.background then return get_highlight_color ("Normal", "background") end
  return vim.o.background == "dark" and "#000000" or "#ffffff"
end

---@diagnostic disable: missing-fields
---@type vim.lsp.config
local config = {
  cmd = {
    "clangd",

    -- Some options are set to their defaults instead of being omitted.
    -- This is to avoid unexpected changes from upstream.
    --
    "--all-scopes-completion=true",
    "--background-index=true",
    "--background-index-priority=normal",
    "--clang-tidy=true",
    "--completion-parse=always",
    "--ranking-model=decision_forest",
    "--completion-style=bundled",
    "--fallback-style=GNU",
    "--function-arg-placeholders=0",
    "--header-insertion=never",
    "--pch-storage=memory",
    "--parse-forwarding-functions",
  },

  capabilities = {
    textDocument = {
      inactiveRegionsCapabilities = {
        inactiveRegions = true,
      },
    },
  },

  -- NOTE: This is experimental, and I should eventually clean this up, but my
  -- hope is that Neovim will support dimming ranges natively in the future, so
  -- that I can remove this code and avoid the need for full Treesitter node
  -- traversal (both for inactive regions and for inactive variables, among
  -- other things).
  --
  handlers = {
    ---Highlights inactive regions.
    ---@param message table Message containing the document URI and regions.
    ["textDocument/inactiveRegions"] = function (_, message, _, _)
      local uniform_resource_identifier = message.textDocument.uri
      local filename = vim.uri_to_fname (uniform_resource_identifier)
      local regions = message.regions

      if #regions == 0 or vim.fn.bufexists (filename) == 0 then
        return
      end

      local buffer = vim.fn.bufadd (filename)
      vim.api.nvim_buf_clear_namespace (buffer, inactive_regions, 0, -1)
      local background_color = get_background_color ()

      for _, region in ipairs (regions) do
        local start_line = region.start.line
        local end_line = region["end"].line

        local parser = vim.treesitter.get_parser (buffer)
        if not parser then
          return
        end
        local tree = parser:parse ()[1]
        if not tree then
          return
        end
        local query = vim.treesitter.query.get (parser:lang (), "highlights")
        if not query then
          return
        end

        local cache = {}

        for id, node in query:iter_captures (tree:root (), buffer, start_line, end_line + 1) do
          local start_row, start_col, end_row, end_col = node:range ()

          -- We only want to highlight the region if it's within our range.
          --
          if start_row >= start_line and start_row <= end_line then
            local highlight_captures = vim.treesitter.get_captures_at_pos (buffer, start_row, start_col)

            for _, highlight in ipairs (highlight_captures) do
              if highlight.capture then
                local highlight_group = "inactive_" .. highlight.capture

                if not cache[highlight_group] then
                  local highlight_color = get_highlight_color ("@" .. highlight.capture)
                  local blended_color = blend_colors (highlight_color, background_color, 0.5)

                  vim.api.nvim_set_hl (0, highlight_group, { fg = blended_color, default = true })
                  cache[highlight] = true
                end
                vim.api.nvim_buf_add_highlight (buffer, inactive_regions, highlight_group, start_row, start_col, end_col)
              end
            end
          end
        end
      end
    end,
  },

  filetypes = {
    "c",
    "cpp",
  },

  root_markers = {
    ".clangd",
    "compile_commands.json",
  },
}

return config
