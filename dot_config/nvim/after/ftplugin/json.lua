if vim.bo.filetype ~= "json" then return end
if vim.fn.executable("clang-format") == 1 then
  vim.opt_local.formatprg = "clang-format --assume-filename=.json"
end

-- Treat JSON as JSONC, a simplified JSON format that allows comments and
-- unquoted values delimited by whitespace. Most tooling nowadays treat them
-- interchangeably.
--
vim.bo.filetype = "jsonc"
