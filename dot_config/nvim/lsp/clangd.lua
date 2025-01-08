---@class ASTViewer
---@field node_pos table Stores node positions for highlighting
---@field detail_pos table Stores detail positions for highlighting
---@field ns number Namespace ID for highlights
local ASTViewer = {}
ASTViewer.__index = ASTViewer

--- Constructor
---@return ASTViewer
function ASTViewer:new ()
  local self = setmetatable ({}, ASTViewer)
  self.node_pos = {}
  self.detail_pos = {}
  self.ns = vim.api.nvim_get_namespaces ()["clangd_ast"] or vim.api.nvim_create_namespace ("clangd_ast")
  return self
end

--- Clears all highlights in the specified buffer.
---@param source_buf number Buffer ID where highlights should be cleared
function ASTViewer:clear_highlight (source_buf)
  vim.api.nvim_buf_clear_namespace (source_buf, self.ns, 0, -1)
end

--- Updates highlights based on the cursor position in the AST buffer.
---@param source_buf number Source buffer ID
---@param ast_buf number AST buffer ID
function ASTViewer:update_highlight (source_buf, ast_buf)
  self:clear_highlight (source_buf)
  if vim.api.nvim_get_current_buf () ~= ast_buf then
    return
  end

  local curline = vim.fn.getcurpos ()[2]
  local curline_ranges = self.node_pos[source_buf][ast_buf][curline]
  if curline_ranges then
    vim.highlight.range (source_buf, self.ns, "Search", curline_ranges.start, curline_ranges["end"], {
      regtype = "v",
      inclusive = false,
      priority = 110,
    })
  end
end

--- Sets up autocommands for handling cursor movement and buffer leave events.
---@param source_buf number Source buffer ID
---@param ast_buf number AST buffer ID
function ASTViewer:setup_hl_autocmd (source_buf, ast_buf)
  local group = vim.api.nvim_create_augroup ("clangd_ast_autocmds", { clear = true })
  vim.api.nvim_create_autocmd ("CursorMoved", {
    group = group,
    buffer = ast_buf,
    callback = function ()
      self:update_highlight (source_buf, ast_buf)
    end,
  })
  vim.api.nvim_create_autocmd ("BufLeave", {
    group = group,
    buffer = ast_buf,
    callback = function ()
      self:clear_highlight (source_buf)
    end,
  })
end

--- Describes a node with its role, kind, and optional details.
---@param role string The role of the node
---@param kind string The kind of the node
---@param detail string|nil Optional detail string
---@return string, table Formatted description and position of details
function ASTViewer:describe (role, kind, detail)
  local str = ""
  local icon = "   " -- Placeholder for icon
  local detailpos = nil
  str = str .. kind

  if not (role == "expression" or role == "statement" or role == "declaration" or role == "template name") then
    str = str .. " " .. role
  end

  if detail then
    detailpos = {
      start = string.len (str) + vim.fn.strlen (icon) + 1,
      ["end"] = string.len (str) + vim.fn.strlen (icon) + string.len (detail) + 1,
    }
    str = str .. " " .. detail
  end

  return (icon .. str), detailpos
end

--- Recursively walks through the AST tree, populating the result table and highlighting buffers.
---@param node table The current AST node
---@param visited table Tracks visited nodes
---@param result table List of formatted lines
---@param padding string Padding string for tree visualization
---@param hl_bufs table Contains source_buf and ast_buf IDs
---@return table Updated result table
function ASTViewer:walk_tree (node, visited, result, padding, hl_bufs)
  visited[node] = true
  local str, detpos = self:describe (node.role, node.kind, node.detail)
  table.insert (result, padding .. str)

  if node.detail and detpos then
    self.detail_pos[hl_bufs.ast_buf][#result] = {
      start = string.len (padding) + detpos.start,
      ["end"] = string.len (padding) + detpos["end"],
    }
  end

  if node.range then
    self.node_pos[hl_bufs.source_buf][hl_bufs.ast_buf][#result] = {
      start = { node.range.start.line, node.range.start.character },
      ["end"] = { node.range["end"].line, node.range["end"].character },
    }
  end

  if node.children then
    vim.iter (node.children):each (function (child)
      if not visited[child] then
        self:walk_tree (child, visited, result, padding .. "  ", hl_bufs)
      end
    end)
  end

  return result
end

--- Highlights details in the AST buffer.
---@param ast_buf number AST buffer ID
function ASTViewer:highlight_detail (ast_buf)
  for linenum, range in pairs (self.detail_pos[ast_buf]) do
    vim.highlight.range (
      ast_buf,
      self.ns,
      "Comment",
      { tonumber (linenum) - 1, range.start },
      { tonumber (linenum) - 1, range["end"] },
      {
        regtype = "v",
        inclusive = false,
        priority = 110,
      }
    )
  end
end

--- Handles the LSP response and updates the AST display.
---@param err string|nil Error message if any
---@param node table AST root node
function ASTViewer:handle_response (err, node)
  if err or not node then
    return
  end

  local source_buf = vim.api.nvim_get_current_buf ()
  local b = vim.b[source_buf]

  if not b.clangd_ast_buf or not vim.api.nvim_buf_is_valid (b.clangd_ast_buf) then
    b.clangd_ast_buf = vim.api.nvim_create_buf (false, true)
    vim.bo[b.clangd_ast_buf].filetype = "ClangdAST"
    vim.bo[b.clangd_ast_buf].shiftwidth = 2
  end

  if not b.clangd_ast_win or not vim.api.nvim_win_is_valid (b.clangd_ast_win) then
    b.clangd_ast_win = vim.api.nvim_open_win (b.clangd_ast_buf, true, { split = "right" })
  else
    vim.cmd (vim.api.nvim_win_get_number (b.clangd_ast_win) .. " wincmd w")
  end

  self.node_pos[source_buf] = self.node_pos[source_buf] or {}
  self.node_pos[source_buf][b.clangd_ast_buf] = {}
  self.detail_pos[b.clangd_ast_buf] = {}

  local lines = self:walk_tree (node, {}, {}, "", { source_buf = source_buf, ast_buf = b.clangd_ast_buf })
  vim.bo.modifiable = true
  vim.api.nvim_buf_set_lines (b.clangd_ast_buf, 0, -1, true, lines)
  vim.bo.modifiable = false
  self:setup_hl_autocmd (source_buf, b.clangd_ast_buf)
  self:highlight_detail (b.clangd_ast_buf)
end

--- Sends a request to the LSP server for AST data.
function ASTViewer:request_ast ()
  vim.lsp.buf_request (0, "textDocument/ast", {
    textDocument = { uri = vim.uri_from_bufnr (0) },
    range = {
      start = { line = 0, character = 0 },
      ["end"] = { line = vim.api.nvim_buf_line_count (0), character = 0 },
    },
  }, function (err, node)
    self:handle_response (err, node)
  end)
end

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

  filetypes = {
    "c", "cpp"
  },

  root_markers = {
    ".clangd", "compile_commands.json"
  },
}

return config
