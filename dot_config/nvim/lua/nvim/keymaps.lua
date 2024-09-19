local keymap = {
  set = vim.keymap.set
}

keymap.set("n", "dd", function()
  return vim.api.nvim_get_current_line():match("^%s*$") and '"_dd' or "dd"
end, { expr = true, desc = "dd (do not yank empty line)" })

return {}
