---@module "mini.tabline"

---@type LazyPluginSpec
local Spec = {
  "mini.tabline", dev = true, dependencies = {{ "mini.icons", dev = true }, { "scope.nvim" }}, event = "BufAdd",

  opts = {
    tabpage_section = "right",
    format = function(buf_id, label)
      local suffix = vim.bo[buf_id].modified and "+ " or ""
      return MiniTabline.default_format(buf_id, label) .. suffix
    end,
  },
}

local unlist_unnamed = function(data)
  local buf = data.buf
  if not (vim.api.nvim_buf_get_name(buf) == "" and vim.bo[buf].buflisted) then return end
  vim.bo[buf].buflisted = false
end

local unlist_unnamed_all = function()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    unlist_unnamed({ buf = buf })
  end
end

vim.api.nvim_create_autocmd({ "VimEnter" }, { once = true, callback = unlist_unnamed_all })

return Spec
