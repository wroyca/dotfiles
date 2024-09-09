---@module "bqf"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-bqf", ft = "qf",

  ---@type BqfConfig
  ---@diagnostic disable: missing-fields
  opts = {
    auto_resize_height = true,
    preview = {
      border = "single",
      show_title = false,
    },
  },
}

---Format entries for the quickfix or location list.
---@param info table Information about the quickfix or location list.
---@return table A list of formatted entries.
function _G.format_quickfix_entries(info)
  local items = _G.retrieve_items(info)
  local formatted_entries = {}

  for i = info.start_idx, info.end_idx do
    local entry = items[i]
    local formatted_entry = _G.format_entry(entry)
    table.insert(formatted_entries, formatted_entry)
  end

  return formatted_entries
end

---Retrieve items from the quickfix or location list.
---@param info table Information about the quickfix or location list.
---@return table A list of items.
function _G.retrieve_items(info)
  if info.quickfix == 1 then
    return vim.fn.getqflist({ id = info.id, items = 0 }).items
  else
    return vim.fn.getloclist(info.winid, { id = info.id, items = 0 }).items
  end
end

---Format a single entry from the quickfix or location list.
---@param entry table The entry to format.
---@return string The formatted entry string.
function _G.format_entry(entry)
  local format_string = "%s:%d:%d:%s%s"
  local filename = ""
  local formatted_entry

  if entry.valid == 1 then
    if entry.bufnr > 0 then
      filename = vim.fn.fnamemodify(vim.fn.bufname(entry.bufnr), ":p")
      if filename == "" then filename = "[No Name]" end
    end

    local line_number = entry.lnum > 99999 and -1 or entry.lnum
    local column_number = entry.col > 999 and -1 or entry.col
    local entry_type = entry.type == "" and "" or " " .. entry.type:sub(1, 1):upper()

    formatted_entry = format_string:format(filename, line_number, column_number, entry_type, entry.text)
  else
    formatted_entry = entry.text
  end

  return formatted_entry
end

vim.o.quickfixtextfunc = "{info -> v:lua._G.format_quickfix_entries(info)}"

return Spec
