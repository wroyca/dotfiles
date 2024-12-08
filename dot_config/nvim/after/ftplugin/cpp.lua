if vim.bo.filetype ~= "cpp" then
	return
end

-- In Neovim, the cinkeys option controls the keys that trigger automatic
-- reindenting when using the smartindent or cindent features. The default
-- behavior for C-like languages includes : as a trigger key, meaning typing :
-- (e.g., in a case label or after an access modifier) will cause automatic
-- reindentation.
--
-- For C++, this default behavior can be problematic because : is commonly used
-- in contexts where reindentation is not desirable.
--
vim.opt_local.cinkeys:remove (":")
