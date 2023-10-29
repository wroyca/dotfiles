local M = {}

---Try to add inlay hints for a specific LSP client and buffer.
---@param client table
-- @param buffer integer
function M.try_add_inlay(client, buffer)
  if client.supports_method([[textDocument/inlayHint]]) then
    --  Protected call as supports_method is not always sufficient.
    --
    pcall(vim.lsp.inlay_hint, buffer, true)
    vim.api.nvim_create_autocmd({ [[InsertEnter]] }, {
      buffer = buffer,
      callback = function()
        pcall(vim.lsp.inlay_hint, buffer, true)
      end
    })
    vim.api.nvim_create_autocmd({ [[InsertLeave]] }, {
      buffer = buffer,
      callback = function()
        pcall(vim.lsp.inlay_hint, buffer, false)
      end
    })
  end
end

---Try to associate an LSP client with buffers of a given filetype.
---@param client string
---@param ft string
function M.try_add_wrapper(client, ft)
  for _, buf in pairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) then
      local ftype = vim.api.nvim_get_option_value([[filetype]], { buf = buf })
      if ftype == ft then
        client.manager:try_add_wrapper(buf)
      end
    end
  end
end

return M
