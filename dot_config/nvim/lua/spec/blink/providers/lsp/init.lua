--- LSP provider transformer dispatcher for blink.cmp
---
--- Route completion item transformations
--- to language server-specific handlers. That is, examines the LSP clients attached
--- to the current buffer and delegates transformation to the appropriate
--- provider module.
---

local M = {}

--- Registry of LSP server name to transformer module mappings
--- @type table<string, { transform_items: function }>
local transformers = {
  clangd = require("spec.blink.providers.lsp.clangd"),
}

--- Dispatches completion item transformation to the appropriate LSP provider.
---
--- This function examines all LSP clients attached to the current buffer and
--- routes the transformation request to a language server-specific handler
--- if one is registered.
---
--- Dispatch Strategy:
---   1. Retrieve all LSP clients attached to the context buffer
---   2. For each client, check if a transformer is registered for that server
---   3. Use the first matching transformer found
---   4. Fall back to returning items unmodified if no transformer matches
---
--- Performance Notes:
---   - Transformer lookup is O(1) via hash table
---   - Client iteration is typically 1-3 iterations (most buffers have few LSP clients)
---   - Early return on first match minimizes overhead
---
--- @param ctx blink.cmp.Context Completion context from blink.cmp
--- @param items blink.cmp.CompletionItem[] Array of completion items from LSP
--- @return blink.cmp.CompletionItem[] Transformed completion items
function M.transform_items(ctx, items)
  if not ctx.bufnr then
    return items
  end

  local clients = vim.lsp.get_clients({ bufnr = ctx.bufnr })

  for _, client in ipairs(clients) do
    local transformer = transformers[client.name]
    if transformer and transformer.transform_items then
      return transformer.transform_items(ctx, items)
    end
  end

  return items
end

return M
