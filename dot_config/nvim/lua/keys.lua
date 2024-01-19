vim.keymap.set([[x]], [[p]],      [['pgv"'.v:register.'y`>']], { silent = true, expr = true })
vim.keymap.set([[x]], [[P]],      [['Pgv"'.v:register.'y`>']], { silent = true, expr = true })
vim.keymap.set([[n]], [[j]],      [[v:count?'j':'gj']],        { silent = true, expr = true })
vim.keymap.set([[n]], [[k]],      [[v:count?'k':'gk']],        { silent = true, expr = true })
vim.keymap.set([[n]], [[<Down>]], [[v:count?'j':'gj']],        { silent = true, expr = true })
vim.keymap.set([[n]], [[<Up>]],   [[v:count?'k':'gk']],        { silent = true, expr = true })
vim.keymap.set([[x]], [[<]],      [[<gv]],                     { silent = true })
vim.keymap.set([[x]], [[>]],      [[>gv]],                     { silent = true })
vim.keymap.set([[n]], [[x]],      [["_x]],                     { silent = true })
vim.keymap.set([[n]], [[X]],      [["_X]],                     { silent = true })
vim.keymap.set([[x]], [[x]],      [["_x]],                     { silent = true })
vim.keymap.set([[x]], [[X]],      [["_X]],                     { silent = true })
vim.keymap.set([[i]], [[,]],      [[,<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[.]],      [[.<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[;]],      [[;<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[{]],      [[{<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[}]],      [[}<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [["]],      [["<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[']],      [['<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[<]],      [[<<c-g>u]],                 { silent = true })
vim.keymap.set([[i]], [[>]],      [[><c-g>u]],                 { silent = true })
vim.keymap.set([[i]], "[",        "[<c-g>u",                   { silent = true })
vim.keymap.set([[i]], "]",        "]<c-g>u",                   { silent = true })

-- Avoid triggering accidental keystrokes by disabling built-in keybindings we don't use.
--
vim.keymap.set([[n]],                  [[<C-Y>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll up a line]] })
vim.keymap.set([[n]],                  [[<C-E>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll down a line]] })
vim.keymap.set([[n]],                  [[<C-B>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll up a page]] })
vim.keymap.set([[n]],                  [[<C-F>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll down a page]] })
vim.keymap.set([[n]],                  [[<C-U>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll up half-screen]] })
vim.keymap.set([[n]],                  [[<C-D>]],       [[<Nop>]],                 { silent = true, desc = [[Scroll down half-screen]] })
vim.keymap.set([[n]],                  [[zH]],          [[<Nop>]],                 { silent = true, desc = [[Scroll half-screen to the right]] })
vim.keymap.set([[n]],                  [[zL]],          [[<Nop>]],                 { silent = true, desc = [[Scroll half-screen to the left]] })
vim.keymap.set([[n]],                  [[zh]],          [[<Nop>]],                 { silent = true, desc = [[Scroll screen left]] })
vim.keymap.set([[n]],                  [[zl]],          [[<Nop>]],                 { silent = true, desc = [[Scroll screen right]] })
vim.keymap.set([[n]],                  [[zs]],          [[<Nop>]],                 { silent = true, desc = [[Scroll screen right w/o moving cursor]] })
vim.keymap.set([[n]],                  [[ze]],          [[<Nop>]],                 { silent = true, desc = [[Scroll screen left w/o moving cursor]] })
vim.keymap.set([[n]],                  [[z-]],          [[<Nop>]],                 { silent = true, desc = [[Redraw current line at bottom of window]] })
vim.keymap.set([[n]],                  [[zb]],          [[<Nop>]],                 { silent = true, desc = [[Redraw current line at bottom of window leaving cursor]] })
vim.keymap.set([[n]],                  [[z.]],          [[<Nop>]],                 { silent = true, desc = [[Redraw current line at center of window]] })
vim.keymap.set([[n]],                  [[zz]],          [[<Nop>]],                 { silent = true, desc = [[Redraw current line at center of window leaving cursor]] })
vim.keymap.set([[n]],                  [[z<CR>]],       [[<Nop>]],                 { silent = true, desc = [[Redraw current line at top of window]] })
vim.keymap.set([[n]],                  [[zt]],          [[<Nop>]],                 { silent = true, desc = [[Redraw current line at top of window leaving cursor]] })
vim.keymap.set([[n]],                  [[z^]],          [[<Nop>]],                 { silent = true, desc = [[Scroll up, w/ startofline]] })
vim.keymap.set([[n]],                  [[z+]],          [[<Nop>]],                 { silent = true, desc = [[Scroll down, w/ startofline]] })
vim.keymap.set([[n]],                  [[go]],          [[<Nop>]],                 { silent = true, desc = [[Go to n-th byte of buffer (default: 0)]] })
vim.keymap.set([[n]],                  [[H]],           [[<Nop>]],                 { silent = true, desc = [[Go to top of screen]] })
vim.keymap.set([[n]],                  [[M]],           [[<Nop>]],                 { silent = true, desc = [[Go to middle of screen]] })
vim.keymap.set([[n]],                  [[L]],           [[<Nop>]],                 { silent = true, desc = [[Go to bottom of screen]] })
vim.keymap.set([[n]],                  [[<C-T>]],       [[<Nop>]],                 { silent = true, desc = [[Jump to previous tag]] })
vim.keymap.set([[n]],                  [[<C-]>]],       [[<Nop>]],                 { silent = true, desc = [[Jump to tag under cursor]] })
vim.keymap.set([[n]],                  [[<Tab>]],       [[<Nop>]],                 { silent = true, desc = [[Go to newer entry in jump list]] })
vim.keymap.set([[n]],                  [[g]],           [[<Nop>]],                 { silent = true, desc = [[Like <C-]> but with :tselect]] })
vim.keymap.set([[n]],                  [[gf]],          [[<Nop>]],                 { silent = true, desc = [[Open file under cursor]] })
vim.keymap.set([[n]],                  [[gF]],          [[<Nop>]],                 { silent = true, desc = [[Open file under cursor and go to line number]] })
vim.keymap.set([[n]],                  [[gd]],          [[<Nop>]],                 { silent = true, desc = [[Go to local declaration]] })
vim.keymap.set([[n]],                  [[gD]],          [[<Nop>]],                 { silent = true, desc = [[Go to global declaration]] })
vim.keymap.set([[n]],                  [[g<C-]>]],      [[<Nop>]],                 { silent = true, desc = [[Tjump to tag under cursor]] })
vim.keymap.set([[n]],                  [[<C-^>]],       [[<Nop>]],                 { silent = true, desc = [[Edit alternate file]] })
vim.keymap.set([[n]],                  [[<C-6>]],       [[<Nop>]],                 { silent = true, desc = [[Edit alternate file]] })
vim.keymap.set([[n]],                  [[g;]],          [[<Nop>]],                 { silent = true, desc = [[Go to previous position in change list]] })
vim.keymap.set([[n]],                  [[g,]],          [[<Nop>]],                 { silent = true, desc = [[Go to next position in change list]] })
vim.keymap.set([[n]],                  [[gh]],          [[<Nop>]],                 { silent = true, desc = [[Select mode (like v)]] })
vim.keymap.set([[n]],                  [[gH]],          [[<Nop>]],                 { silent = true, desc = [[Select mode (like V)]] })
vim.keymap.set([[n]],                  [[g<C-H>]],      [[<Nop>]],                 { silent = true, desc = [[Select mode (like <C-v>)]] })
vim.keymap.set([[n]],                  [[gV]],          [[<Nop>]],                 { silent = true, desc = [[Avoid automatic reselection after select mode mapping]] })
vim.keymap.set([[n]],                  [[c]],           [[<Nop>]],                 { silent = true, desc = [[Change to {motion}]] })
vim.keymap.set([[n]],                  [[cc]],          [[<Nop>]],                 { silent = true, desc = [[Change line]] })
vim.keymap.set([[n]],                  [[C]],           [[<Nop>]],                 { silent = true, desc = [[Change to end of line]] })
vim.keymap.set([[n]],                  [[s]],           [[<Nop>]],                 { silent = true, desc = [[Change character]] })
vim.keymap.set([[n]],                  [[S]],           [[<Nop>]],                 { silent = true, desc = [[Change line]] })
vim.keymap.set({[[n]], [[x]]},         [[q]],           [[<Nop>]],                 { silent = true, desc = [[Record a macro in {register}]] })
vim.keymap.set({[[n]], [[x]]},         [[@]],           [[<Nop>]],                 { silent = true, desc = [[Execute macro {register}]] })
vim.keymap.set({[[n]], [[x]]},         [[@@]],          [[<Nop>]],                 { silent = true, desc = [[Repeat last macro]] })
vim.keymap.set({[[n]], [[x]]},         [[Q]],           [[<Nop>]],                 { silent = true, desc = [[Replay last macro (nvim) or Ex mode (vim)]] })
vim.keymap.set({[[n]], [[x]]},         [[gQ]],          [[<Nop>]],                 { silent = true, desc = [[Switch to ex mode]] })
vim.keymap.set({[[n]], [[x]], [[i]]},  [[<F1>]],        [[<Nop>]],                 { silent = true, desc = [[Help page]] })
vim.keymap.set({[[n]], [[x]]},         [[q:]],          [[<Nop>]],                 { silent = true, desc = [[Command-line window]] })

-- https://blog.jetbrains.com/idea/2014/07/the-backspace-key-gets-smarter-in-intellij-idea-14-eap/
--
local win_get_cursor   = vim.api.nvim_win_get_cursor
local get_current_line = vim.api.nvim_get_current_line
local buf_get_lines    = vim.api.nvim_buf_get_lines

function smart_backspace()
  local line, column = table.unpack (win_get_cursor (0))
  local char         = get_current_line ():sub (column, column)
  local prev_line    = buf_get_lines (0, line - 2, line - 1, false) [1]

  if char == [[ ]] and prev_line and prev_line:match ("^%s*$") then
    -- Here be dragons.
    vim.cmd([[
      let g:exprvalue = (&indentexpr isnot '' ? &indentkeys : &cinkeys) =~? '!\^F' &&
        \ &backspace =~? '.*eol\&.*start\&.*indent\&' &&
        \ !search('\S','nbW',line('.')) ? (col('.') != 1 ? "\<C-U>" : "") .
        \ "\<bs>" . (getline(line('.')-1) =~ '\S' ? "" : "\<C-F>") : "\<bs>"
    ]])
    return vim.g.exprvalue
  else
    return vim.api.nvim_replace_termcodes([[<bs>]], true, false, true)
  end
end

function smart_cursor (direction)
  local line, column = table.unpack (win_get_cursor (0))
  local char         = get_current_line ():sub (column, column)
  local next_line

  if direction == [[up]] then
    next_line = buf_get_lines (0, line - 2, line - 1, false) [1]
  else
    next_line = buf_get_lines (0, line, line + 1, false) [1]
  end

  if char == [[ ]] and next_line and next_line:match("^%s*$") then
    -- Here be dragons.
    vim.cmd([[
    let g:exprvalue = (&indentexpr isnot '' ? &indentkeys : &cinkeys) =~? '!\^F' &&
      \ &backspace =~? '.*eol\&.*start\&.*indent\&' &&
      \ !search('\S','nbW',line('.')) ?
      \   (col('.') != 1 && getline('.') !~ '^\s*$' ? "" : "\<C-U>") .
      \   "\<]] ..
      direction .. [[>" . (getline(line('.')+(]] .. (direction == "up" and "-1" or "+1") .. [[)) =~ '\S' ? "" : "\<C-F>") :
      \ "\<]] .. direction .. [[>"
    ]])
    return vim.g.exprvalue
  else
    return vim.api.nvim_replace_termcodes([[<]] .. direction .. [[>]], true, false, true)
  end
end

vim.keymap.set([[i]], [[<Up>]],   [[v:lua.smart_cursor("up")]],   { expr = true, noremap = true, replace_keycodes = false })
vim.keymap.set([[i]], [[<Down>]], [[v:lua.smart_cursor("down")]], { expr = true, noremap = true, replace_keycodes = false })
vim.keymap.set([[i]], [[<BS>]],   [[v:lua.smart_backspace()]],    { expr = true, noremap = true, replace_keycodes = false })

return {}
