if vim.bo.filetype ~= "cpp" then
  return
end

-- Cinkeys controls which keys trigger automatic reindenting when using
-- smartindent or cindent features. Default behavior for C-like languages
-- includes : as a trigger key, meaning typing : (e.g., in case label or after
-- an access modifier) will cause automatic reindentation.

vim.opt_local.cinkeys:remove (":")
