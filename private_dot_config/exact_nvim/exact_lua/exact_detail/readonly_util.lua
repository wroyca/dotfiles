local M = {}

---Try to add inlay hints for a specific LSP client and buffer.
---@param client table
-- @param buffer integer
function M.try_add_inlay(client, buffer)
  if client.supports_method([[textDocument/inlayHint]]) then
    -- Protected call as supports_method is not always sufficient.
    pcall(vim.lsp.inlay_hint, buffer, true)
  end
end

---Try to associate an LSP client with buffers of a given filetype.
---@param client string
---@param ft string
function M.try_add_wrapper(client, ft)
  for _, buf in pairs(vim.api.nvim_list_bufs()) do
		if vim.api.nvim_buf_is_loaded(buf) then
			local ftype = vim.api.nvim_buf_get_option(buf, [[filetype]])
			if ftype == ft then
				client.manager:try_add_wrapper(buf)
			end
		end
	end
end

return M
