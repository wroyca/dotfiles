if vim.bo.filetype ~= "cpp" then
  return
end

-- By default, Vim's `cindent` implementation treats the colon as a trigger for
-- re-indentation, assuming it signifies a label, a `case` statement, or an
-- access specifier. While this heuristic is well-intentioned, it becomes a
-- nuisance in C++ due to the ubiquity of the scope resolution operator (`::`).
--
-- The issue is that the logic triggers on the first colon. So when we type
-- `std::`, the editor attempts to re-indent the line immediately after the
-- first keystroke. This results in a jarring visual jump.
--
-- We are removing the colon from the input keys that trigger indentation
-- (`cinkeys`). This does mean we lose the "electric" behavior for `public:`
-- and `case:`, but this is an acceptable compromise. Since we have
-- `clang-format` wired up to `gq` by default, we rely on it for canonical
-- style enforcement anyway.
--
vim.opt_local.cinkeys:remove (":")
