if vim.bo.filetype ~= "json" then
	return
end

-- JSONC (JSON with Comments) is an extended version of JSON that allows
-- comments (// or /* */) and unquoted keys/values, making it more
-- human-readable and easier to work with in configuration files. While
-- standard JSON does not permit these features, many modern tools and editors
-- treat JSON and JSONC interchangeably because JSONC simplifies configuration
-- management without breaking compatibility.
--
vim.bo.filetype = "jsonc"
