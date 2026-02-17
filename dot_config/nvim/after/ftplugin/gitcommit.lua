if vim.bo.filetype ~= "gitcommit" then
  return
end

-- Disable smart indentation. The heuristics used by Vim (indenting after `{`,
-- outdenting on `}`) are geared towards C-like source code. In a commit
-- message, where we are writing free-form prose or pasting patches, these
-- heuristics tend to get in the way and cause unexpected jumps. We rely on
-- autoindent instead, which simply copies the indentation from the previous
-- line. this is sufficient for bulleted lists and simpler for prose.
--
vim.opt_local.smartindent = false

-- Mark the buffer as unlisted. Conceptually, a commit message is a temporary
-- artifact: we write it, save it, and the editor process usually exits or
-- closes the window. We don't want these ephemeral buffers cluttering the
-- buffer list (`:ls`) or interrupting the flow when cycling through actual
-- project files (`:bnext`/`:bprev`).
--
vim.opt_local.buflisted = false
