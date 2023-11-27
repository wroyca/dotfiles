local M = {}
local key = require [[detail.key]]

local function undo_break_point(src)
  return src[1] .. [[<C-g>u]]
end

local anchors = {
  { [[,]],         [[@undo_break_point]] },
  { [[.]],         [[@undo_break_point]] },
  { [[;]],         [[@undo_break_point]] },
  { [[{]],         [[@undo_break_point]] },
  { [[}]],         [[@undo_break_point]] },
  { [["]],         [[@undo_break_point]] },
  { [[']],         [[@undo_break_point]] },
  { [[<]],         [[@undo_break_point]] },
  { [[>]],         [[@undo_break_point]] },
  { "[",           [[@undo_break_point]] },
  { "]",           [[@undo_break_point]] },
  { [[<space>]],   [[@undo_break_point]] },
  { [[<cr>]],      [[@undo_break_point]] },

  { [[<S-CR>]],    [[@cmp.complete]] },
  { [[<Tab>]],     [[@cmp.confirm_insert]] },
  { [[<S-Tab>]],   [[@cmp.confirm_replace]] },
  --{ [[<M-p>]],     [[@cmp.snip_previous]],   mode = { [[n]], [[x]], [[v]] } },
  --{ [[<M-n>]],     [[@cmp.snip_next]],       mode = { [[n]], [[x]], [[v]] } },
  { [[<C-p>]],     [[@cmp.select_prev_item]] },
  { [[<C-n>]],     [[@cmp.select_next_item]] },
  { [[<S-p>]],     [[@cmp.scroll_docs_up]] },
  { [[<S-n>]],     [[@cmp.scroll_docs_down]] },

  -- basic movement
  { "h",           [[<Nop>]],                [[go left]] },
  { "h",           [[<Nop>]],                [[go left]] },
  { "j",           [[<Nop>]],                [[go down]] },
  { "k",           [[<Nop>]],                [[go up]] },
  { "l",           [[<Nop>]],                [[go right]] },
  { "<C-P>",       [[<Nop>]],                [[go up]] },
  { "<C-H>",       [[<Nop>]],                [[go left]] },
  { "<C-J>",       [[<Nop>]],                [[go down]] },
  { "<C-N>",       [[<Nop>]],                [[go down]] },
  { "<C-M>",       [[<Nop>]],                [[go down]] },

  -- word movement
  { "w",           [[<Nop>]],                [[go to start of next word]] },
  { "e",           [[<Nop>]],                [[go to end of next word]] },
  { "b",           [[<Nop>]],                [[go to start of previous word]] },
  { "ge",          [[<Nop>]],                [[go to end of previous word]] },
  { "W",           [[<Nop>]],                [[go to start of next WORD]] },
  { "E",           [[<Nop>]],                [[go to end of next WORD]] },
  { "B",           [[<Nop>]],                [[go to start of previous WORD]] },
  { "gE",          [[<Nop>]],                [[go to end of previous WORD]] },

  -- line movement
  { "+",           [[<Nop>]],                [[go to next line (first non-blank)]] },
  { "-",           [[<Nop>]],                [[go to previous line (first non-blank)]] },
  { "0",           [[<Nop>]],                [[go to first char of line]] },
  { "|",           [[<Nop>]],                [[go to first column of line]] },
  { "$",           [[<Nop>]],                [[go to end of line]] },
  { "{",           [[<Nop>]],                [[go back a paragraph]] },
  { "}",           [[<Nop>]],                [[go forward a paragraph]] },
  { "(",           [[<Nop>]],                [[go back a sentence]] },
  { ")",           [[<Nop>]],                [[go forward a sentence]] },
  { "^",           [[<Nop>]],                [[go to first non-blank character of line]] },
  { "gM",          [[<Nop>]],                [[go to middle of line]] },
  { "_",           [[<Nop>]],                [[go to first non-blank character of line]] },
  { "g_",          [[<Nop>]],                [[go to last non-blank character of line]] },

  -- line wrap movement
  { "g$",          [[<Nop>]],                [[go to last character of line (line wrap)]] },
  { "g0",          [[<Nop>]],                [[go to first character of line (line wrap)]] },
  { "g^",          [[<Nop>]],                [[go to first non-blank character on line (line wrap)]] },
  { "gj",          [[<Nop>]],                [[go to next line (line wrap)]] },
  { "gk",          [[<Nop>]],                [[go to previous line (line wrap)]] },
  { "gm",          [[<Nop>]],                [[go to middle of line (line wrap)]] },

  -- scrolling
  { "<C-Y>",       [[<Nop>]],                [[scroll up a line]] },
  { "<C-E>",       [[<Nop>]],                [[scroll down a line]] },
  { "<C-B>",       [[<Nop>]],                [[scroll up a page]] },
  { "<C-F>",       [[<Nop>]],                [[scroll down a page]] },
  { "<C-U>",       [[<Nop>]],                [[scroll up half-screen]] },
  { "<C-D>",       [[<Nop>]],                [[scroll down half-screen]] },
  { "zH",          [[<Nop>]],                [[scroll half-screen to the right]] },
  { "zL",          [[<Nop>]],                [[scroll half-screen to the left]] },
  { "zh",          [[<Nop>]],                [[scroll screen left]] },
  { "zl",          [[<Nop>]],                [[scroll screen right]] },
  { "zs",          [[<Nop>]],                [[scroll screen right w/o moving cursor]] },
  { "ze",          [[<Nop>]],                [[scroll screen left w/o moving cursor]] },

  -- recenter screen
  { "z-",          [[<Nop>]],                [[redraw current line at bottom of window]] },
  { "zb",          [[<Nop>]],                [[redraw current line at bottom of window leaving cursor]] },
  { "z.",          [[<Nop>]],                [[redraw current line at center of window]] },
  { "zz",          [[<Nop>]],                [[redraw current line at center of window leaving cursor]] },
  { "z<CR>",       [[<Nop>]],                [[redraw current line at top of window]] },
  { "zt",          [[<Nop>]],                [[redraw current line at top of window leaving cursor]] },
  { "z^",          [[<Nop>]],                [[scroll up, w/ startofline]] },
  { "z+",          [[<Nop>]],                [[scroll down, w/ startofline]] },

  -- file movement
  { "G",           [[<Nop>]],                [[go to end of file]] },
  { "gg",          [[<Nop>]],                [[go to first line]] },
  { "go",          [[<Nop>]],                [[go to n-th byte of buffer (default: 0)]] },

  -- screen movement
  { "H",           [[<Nop>]],                [[go to top of screen]] },
  { "M",           [[<Nop>]],                [[go to middle of screen]] },
  { "L",           [[<Nop>]],                [[go to bottom of screen]] },

  -- f and t
  { ",",           [[<Nop>]],                [[repeat f/F/t/T in opposite direction]] },
  { ";",           [[<Nop>]],                [[repeat last f/F/t/T]] },
  { "f",           [[<Nop>]],                [[find char forward]] },
  { "t",           [[<Nop>]],                [[till character forward]] },
  { "F",           [[<Nop>]],                [[find char backward]] },
  { "T",           [[<Nop>]],                [[till character backward]] },

  -- jumping
  { "<C-I>",       [[<Nop>]],                [[jump forwards]] },
  { "<C-O>",       [[<Nop>]],                [[jump backward]] },
  { "<C-T>",       [[<Nop>]],                [[jump to previous tag]] },
  { "<C-]>",       [[<Nop>]],                [[jump to tag under cursor]] },
  { "<Tab>",       [[<Nop>]],                [[go to newer entry in jump list]] },
  { "g]",          [[<Nop>]],                [[like <C-]> but with :tselect]] },
  { "gf",          [[<Nop>]],                [[open file under cursor]] },
  { "gF",          [[<Nop>]],                [[open file under cursor and go to line number]] },
  { "gd",          [[<Nop>]],                [[go to local declaration]] },
  { "gD",          [[<Nop>]],                [[go to global declaration]] },
  { "g<C-]>",      [[<Nop>]],                [[tjump to tag under cursor]] },
  { "<C-^>",       [[<Nop>]],                [[edit alternate file]] },
  { "<C-6>",       [[<Nop>]],                [[edit alternate file]] },

  -- change list
  { 'g;',          [[<Nop>]],                [[go to previous position in change list]] },
  { 'g,',          [[<Nop>]],                [[go to next position in change list]] },

  -- visual mode
  { "V",           [[<Nop>]],                [[visual line mode]] },
  { "v",           [[<Nop>]],                [[visual mode]] },
  { "<C-V>",       [[<Nop>]],                [[visual block mode]] },
  { "gv",          [[<Nop>]],                [[select previous visual selection]] },
  { "<C-Q>",       [[<Nop>]],                [[visal block mode (for GUIs because <C-v> may be paste)]] },

  -- select mode
  { "gh",          [[<Nop>]],                [[select mode (like v)]] },
  { "gH",          [[<Nop>]],                [[select mode (like V)]] },
  { "g<C-H>",      [[<Nop>]],                [[select mode (like <C-v>)]] },
  { "gV",          [[<Nop>]],                [[avoid automatic reselection after select mode mapping]] },

  -- insert mode
  --{ "i",           [[<Nop>]],            [[insert]] },
  { "I",           [[<Nop>]],                [[insert to beginning of line]] },
  { "gi",          [[<Nop>]],                [[insert where last exited insert mode]] },
  { "gI",          [[<Nop>]],                [[insert to begining of line (before indent)]] },
  --{ "a",           [[<Nop>]],            [[append after cursor]] },
  { "A",           [[<Nop>]],                [[append to end of line]] },
  { "o",           [[<Nop>]],                [[open a new line below]] },
  { "O",           [[<Nop>]],                [[open a new line above]] },

  -- change
  { "c",           [[<Nop>]],                [[change to {motion}]] },
  { "cc",          [[<Nop>]],                [[change line]] },
  { "C",           [[<Nop>]],                [[change to end of line]] },
  { "s",           [[<Nop>]],                [[change character]] },
  { "S",           [[<Nop>]],                [[change line]] },

  -- delete
  { "d",           [[<Nop>]],                [[delete to {motion}]] },
  { "dd",          [[<Nop>]],                [[delete line]] },
  { "D",           [[<Nop>]],                [[delete to end of line]] },
  { "x",           [[<Nop>]],                [[delete character]] },
  { "X",           [[<Nop>]],                [[delete character before cursor]] },
  { "<Del>",       [[<Nop>]],                [[delete character under cursor]] },

  -- yank
  { "y",           [[<Nop>]],                [[yank to {motion}]] },
  { "yy",          [[<Nop>]],                [[yank a line]] },
  { "Y",           [[<Nop>]],                [[yank to end of line (nvim)]] },
  { "zy",          [[<Nop>]],                [[yank w/o trailing whitespace (for blocks)]] },

  -- paste
  { "p",           [[<Nop>]],                [[put text after cursor]] },
  { "P",           [[<Nop>]],                [[put before cursor]] },
  { "gp",          [[<Nop>]],                [[like p, but leave cursor just after new text]] },
  { "gP",          [[<Nop>]],                [[like P, but leave cursor just after new text]] },
  { "zp",          [[<Nop>]],                [[like p, but w/o trailing whitespace in block]] },
  { "zP",          [[<Nop>]],                [[like P, but w/o trailing whitespace in block]] },

  -- replace
  { "r",           [[<Nop>]],                [[replace character under cursor]] },
  { "R",           [[<Nop>]],                [[replace mode]] },
  { "gr",          [[<Nop>]],                [[replace character w/o affecting layout (r but for <Tab>)]] },
  { "gR",          [[<Nop>]],                [[visual replace mode (for <Tab>)]] },


  -- increment and decrement
  { "<C-A>",       [[<Nop>]],                [[increase number after cursor]] },
  { "<C-X>",       [[<Nop>]],                [[decrease number after cursor]] },

  -- repeat
  { ".",           [[<Nop>]],                [[repeat last change]] },

  -- marks
  { "`",           [[<Nop>]],                [[go to {mark}]] },
  { "'",           [[<Nop>]],                [[go to {mark} line]] },
  { "g`",          [[<Nop>]],                [[go to {mark} (keeping jumps)]] },
  { "g'",          [[<Nop>]],                [[go to {mark} line (keeping jumps)]] },
  { "m",           [[<Nop>]],                [[mark current position with {mark}]] },

  -- indent
  { ">",           [[<Nop>]],                [[indent lines {motion}]] },
  { ">>",          [[<Nop>]],                [[indent lines]] },
  { "<lt>",        [[<Nop>]],                [[deindent lines {motion}]] },
  { "<lt><lt>",    [[<Nop>]],                [[dedent line]] },

  -- registers
  { '"',           [[<Nop>]],                [[select {register}]] },

  -- undo and redo
  { "U",           [[<Nop>]],                [[undo changes on one line]] },
  { "u",           [[<Nop>]],                [[undo]] },
  { "g-",          [[<Nop>]],                [[undo (via :earlier)]] },
  { "g+",          [[<Nop>]],                [[redo (via :later)]] },
  { "<C-R>",       [[<Nop>]],                [[redo]] },

  -- macros
  { "q",           [[<Nop>]],                [[record a macro in {register}]] },
  { "@",           [[<Nop>]],                [[execute macro {register}]] },
  { "@@",          [[<Nop>]],                [[repeat last macro]] },
  { "Q",           [[<Nop>]],                [[replay last macro (nvim) or Ex mode (vim)]] },

  -- joining lines
  { "J",           [[<Nop>]],                [[join next line]] },
  { "gJ",          [[<Nop>]],                [[join line (w/o space)]] },

  -- exiting
  { "ZQ",          [[<Nop>]],                [[same as :q!]] },
  { "ZZ",          [[<Nop>]],                [[save and exit]] },

  -- case switching
  { "~",           [[<Nop>]],                [[switch case under cursor]] },
  { "gu",          [[<Nop>]],                [[lowercase to {motion}]] },
  { "gU",          [[<Nop>]],                [[uppercase to {motion}]] },
  { "g~",          [[<Nop>]],                [[switch case to {motion}]] },
  { "g?",          [[<Nop>]],                [[rot13 encode to {motion}]] },
  { 'g?g?',        [[<Nop>]],                [[rot13 encode current line]] },

  -- show stuff under cursor
  { "ga",          [[<Nop>]],                [[show ASCII value under cursor]] },
  { "g8",          [[<Nop>]],                [[show UTF8 sequence of character under cursor]] },
  { "g<C-G>",      [[<Nop>]],                [[show cursor position info]] },
  { "<C-G>",       [[<Nop>]],                [[show current filname and cursor position]] },

  -- diff
  { 'do',          [[<Nop>]],                [[same as :diffget]] },
  { 'dp',          [[<Nop>]],                [[same as :diffput]] },

  -- searching
  { '/',           [[<Nop>]],                [[search forward]] },
  { '?',           [[<Nop>]],                [[search backward]] },
  { '*',           [[<Nop>]],                [[search forward for word under cursor]] },
  { '#',           [[<Nop>]],                [[search backward for word under cursor]] },
  { 'n',           [[<Nop>]],                [[go to next search match]] },
  { 'N',           [[<Nop>]],                [[go to previous search match]] },
  { 'gn',          [[<Nop>]],                [[go to next match and visually select it]] },
  { 'gN',          [[<Nop>]],                [[go to previous match and visually select it]] },
  { "g*",          [[<Nop>]],                [[like *, but without \\< and \\>]] },
  { "g#",          [[<Nop>]],                [[like #, but without \\< and \\>]] },
  { "<C-C>",       [[<Nop>]],                [[interrupt a search]] },

  -- miscellaneous
  { "<Space>",     [[<Nop>]],                [[{same as} l]] },
  { "<CR>",        [[<Nop>]],                [[go to first char on next line]] },
  { "<C-[>",       [[<Nop>]],                [[exit back to normal mode]] },
  { "g<C-A>",      [[<Nop>]],                [[dump memory profile]] },
  --{ ":",           [[<Nop>]],            [[command mode]] },
  { "&",           [[<Nop>]],                [[repeat last :s on current line (w/o options)]] },
  { "g&",          [[<Nop>]],                [[repeat last :s with flags]] },
  { "%",           [[<Nop>]],                [[find forward matching bracket]] },
  { "g%",          [[<Nop>]],                [[find backward matching bracket]] },
  { "<C-L>",       [[<Nop>]],                [[clear and redraw screen]] },
  { "K",           [[<Nop>]],                [[lookup word under cursor]] },
  { "gQ",          [[<Nop>]],                [[switch to ex mode]] },
  { "gs",          [[<Nop>]],                [[sleep for N seconds (default: 1)]] },
  { "<C-Z>",       [[<Nop>]],                [[like :stop]] },
  { "gO",          [[<Nop>]],                [[table of contents (neovim)]] },
  { "g<lt>",       [[<Nop>]],                [[show output of previous command]] },
  { "g@",          [[<Nop>]],                [[call operatorfunc]] },
  { '@:',          [[<Nop>]],                [[repeat previous command]] },
  { 'q:',          [[<Nop>]],                [[edit : in command-line window]] },
  { 'q/',          [[<Nop>]],                [[edit / in command-line window]] },
  { 'q?',          [[<Nop>]],                [[edit ? in command-line window]] },

  -- filtering
  { "!",           [[<Nop>]],                [[filter through command to {motion}]] },
  { "!!",          [[<Nop>]],                [[filter through command]] },
  { "=",           [[<Nop>]],                [[format lines {motion}]] },
  { "==",          [[<Nop>]],                [[format line]] },
  { "gq",          [[<Nop>]],                [[format (w/ formatexpr) lines to {motion}]] },
  { "gw",          [[<Nop>]],                [[format (w/ formatoptions) lines to {motion}]] },

  -- folds
  { "za",          [[<Nop>]],                [[toggle fold under cursor]] },
  { "zA",          [[<Nop>]],                [[toggle fold recursively]] },
  { "zv",          [[<Nop>]],                [[open just enough folds]] },
  { "zx",          [[<Nop>]],                [[reset manually opened/closed folds]] },
  { "zX",          [[<Nop>]],                [[reset manually opened/closed folds and reapply foldexpr]] },
  { "zC",          [[<Nop>]],                [[close all folds under cursor]] },
  { "zD",          [[<Nop>]],                [[delete all folds under cursor]] },
  { "zM",          [[<Nop>]],                [[close all folds (fdl, <Nop>, 0)]] },
  { "zN",          [[<Nop>]],                [[enable folding (set fen)]] },
  { "zO",          [[<Nop>]],                [[open all folds under cursor]] },
  { "zR",          [[<Nop>]],                [[open all folds (fdl, <Nop>, max)]] },
  { "zc",          [[<Nop>]],                [[close fold under cursor]] },
  { "zd",          [[<Nop>]],                [[delete fold under cursor]] },
  { "zE",          [[<Nop>]],                [[eliminate all folds in window]] },
  { "zf",          [[<Nop>]],                [[create a fold]] },
  { "zF",          [[<Nop>]],                [[create a fold linewise]] },
  { "zi",          [[<Nop>]],                [[invert foldenable]] },
  { "zm",          [[<Nop>]],                [[fold more (fdl -= 1)]] },
  { "zn",          [[<Nop>]],                [[disable fold (set nofen)]] },
  { "zo",          [[<Nop>]],                [[open fold under cursor]] },
  { "zr",          [[<Nop>]],                [[reduce folding (fdl += 1)]] },
  { "zk",          [[<Nop>]],                [[go to previous fold]] },
  { "zj",          [[<Nop>]],                [[go to next fold]] },
  { "[z",          [[<Nop>]],                [[go to start of current/containing fold]] },
  { "]z",          [[<Nop>]],                [[go to end of current/containing fold]] },

  -- spellcheck
  { "]s",          [[<Nop>]],                [[go to next misspell]] },
  { "[s",          [[<Nop>]],                [[go to previous misspell]] },
  { "]S",          [[<Nop>]],                [[go to next misspell, but only bad]] },
  { "[S",          [[<Nop>]],                [[go to previous misspell, but only bad]] },
  { "zg",          [[<Nop>]],                [[add word under cursor as good in spellfile]] },
  { "zG",          [[<Nop>]],                [[add word under cursor as good in internal word list]] },
  { "zw",          [[<Nop>]],                [[add word under cursor as bad in spellfile]] },
  { "zW",          [[<Nop>]],                [[add word under cursor as bad in internal word list]] },
  { "zuw",         [[<Nop>]],                [[undo zw]] },
  { "zug",         [[<Nop>]],                [[undo zg]] },
  { "zuW",         [[<Nop>]],                [[undo zW]] },
  { "zuG",         [[<Nop>]],                [[undo zG]] },
  { "z=",          [[<Nop>]],                [[suggest words for word under cursor]] },

  -- nvim terminal commands
  { '<C-\\><C-N>', [[<Nop>]],                [[go to Normal mode (no-op)]] },
  { '<C-\\><C-G>', [[<Nop>]],                [[go to mode specified with \'insertmode\']] },

  -- window commands
  { "<C-W>+",      [[<Nop>]],                [[increase window height]] },
  { "<C-W>-",      [[<Nop>]],                [[descrease window height]] },
  { "<C-W><C-W>",  [[<Nop>]],                [[move cursor to next window]] },
  { "<C-W><lt>",   [[<Nop>]],                [[decrease window width]] },
  { "<C-W>=",      [[<Nop>]],                [[make windows equal height/width]] },
  { "<C-W>>",      [[<Nop>]],                [[increase window width]] },
  { "<C-W>R",      [[<Nop>]],                [[rotate windows upwards]] },
  { "<C-W>W",      [[<Nop>]],                [[move cursor to previous window]] },
  { '<C-W>H',      [[<Nop>]],                [[move window to the far left]] },
  { '<C-W>J',      [[<Nop>]],                [[move window to the very bottom]] },
  { '<C-W>K',      [[<Nop>]],                [[move window to the very top]] },
  { '<C-W>L',      [[<Nop>]],                [[move window to the far right]] },
  { '<C-W>P',      [[<Nop>]],                [[go to preview window]] },
  { '<C-W>S',      [[<Nop>]],                [[{same as} <C-W>s]] },
  { '<C-W>T',      [[<Nop>]],                [[move window to new tab]] },
  { "<C-W>]",      [[<Nop>]],                [[split window and jump to tag under cursor]] },
  { "<C-W>^",      [[<Nop>]],                [[split window and edit alternate file]] },
  { "<C-W>_",      [[<Nop>]],                [[expand window height]] },
  { "<C-W>b",      [[<Nop>]],                [[move cursor to bottom window]] },
  { "<C-W>c",      [[<Nop>]],                [[close window]] },
  { "<C-W>f",      [[<Nop>]],                [[split window and edit filename under cursor (gf)]] },
  { "<C-W>j",      [[<Nop>]],                [[move cursor to window below]] },
  { "<C-W>k",      [[<Nop>]],                [[move cursor to window above]] },
  { "<C-W>n",      [[<Nop>]],                [[new window]] },
  { "<C-W>o",      [[<Nop>]],                [[close other windows]] },
  { "<C-W>p",      [[<Nop>]],                [[move cursor to previous window]] },
  { "<C-W>q",      [[<Nop>]],                [[close window]] },
  { "<C-W>r",      [[<Nop>]],                [[rotate windows down]] },
  { "<C-W>s",      [[<Nop>]],                [[split window horizontally]] },
  { "<C-W>t",      [[<Nop>]],                [[move cursor to top window]] },
  { "<C-W>x",      [[<Nop>]],                [[exchange window with next]] },
  { "<C-W>z",      [[<Nop>]],                [[close preview window]] },
  { '<C-W>F',      [[<Nop>]],                [[split and go to file w/ line (gF)]] },
  { '<C-W>d',      [[<Nop>]],                [[split and go to definition (gd)]] },
  { '<C-W>h',      [[<Nop>]],                [[move cursor to window left]] },
  { '<C-W>i',      [[<Nop>]],                [[split window and jump to declaration of identifier under the cursor]] },
  { '<C-W>l',      [[<Nop>]],                [[move cursor to window right]] },
  { '<C-W>v',      [[<Nop>]],                [[split window vertically]] },
  { '<C-W>w',      [[<Nop>]],                [[move cursor to next window]] },
  { "<C-W>|",      [[<Nop>]],                [[expand window width]] },
  { "<C-W>}",      [[<Nop>]],                [[show tag under cursor in preview window]] },
  { '<C-W>g]',     [[<Nop>]],                [[split and :tselect tag]] },
  { '<C-W>g}',     [[<Nop>]],                [[:ptjump on tag under jursor]] },
  { '<C-W>gf',     [[<Nop>]],                [[edit file under cursor in new tab (gf)]] },
  { '<C-W>gF',     [[<Nop>]],                [[edit file under cursor + line in new tag (gF)]] },
  { '<C-W>gt',     [[<Nop>]],                [[{same as} gt]] },
  { '<C-W>gT',     [[<Nop>]],                [[{same as} gT]] },
  { '<C-W>g<Tab>', [[<Nop>]],                [[{same as} g<Tab>]] },
  { '<C-W><C-B>',  [[<Nop>]],                [[{same as} <C-W>b]] },
  { '<C-W><C-C>',  [[<Nop>]],                [[{same as} <C-W>c]] },
  { '<C-W><C-D>',  [[<Nop>]],                [[{same as} <C-W>d]] },
  { '<C-W><C-F>',  [[<Nop>]],                [[{same as} <C-W>f]] },
  { '<C-W><C-H>',  [[<Nop>]],                [[{same as} <C-W>h]] },
  { '<C-W><C-I>',  [[<Nop>]],                [[{same as} <C-W>i]] },
  { '<C-W><C-J>',  [[<Nop>]],                [[{same as} <C-W>j]] },
  { '<C-W><C-K>',  [[<Nop>]],                [[{same as} <C-W>k]] },
  { '<C-W><C-L>',  [[<Nop>]],                [[{same as} <C-W>l]] },
  { '<C-W><C-N>',  [[<Nop>]],                [[{same as} <C-W>n]] },
  { '<C-W><C-O>',  [[<Nop>]],                [[{same as} <C-W>o]] },
  { '<C-W><C-P>',  [[<Nop>]],                [[{same as} <C-W>p]] },
  { '<C-W><C-Q>',  [[<Nop>]],                [[{same as} <C-W>q]] },
  { '<C-W><C-R>',  [[<Nop>]],                [[{same as} <C-W>r]] },
  { '<C-W><C-S>',  [[<Nop>]],                [[{same as} <C-W>s]] },
  { '<C-W><C-T>',  [[<Nop>]],                [[{same as} <C-W>t]] },
  { '<C-W><C-V>',  [[<Nop>]],                [[{same as} <C-W>v]] },
  { '<C-W><C-X>',  [[<Nop>]],                [[{same as} <C-W>x]] },
  { '<C-W><C-Z>',  [[<Nop>]],                [[{same as} <C-W>z]] },
  { '<C-W><C-]>',  [[<Nop>]],                [[{same as} <C-W>]\]] },
  { '<C-W><C-^>',  [[<Nop>]],                [[{same as} <C-W>^]] },
  { '<C-W><C-_>',  [[<Nop>]],                [[{same as} <C-W>_]] },

  -- tabs
  { "gt",          [[<Nop>]],                [[go to next tab]] },
  { "gT",          [[<Nop>]],                [[go to previous tab]] },
  { "g<Tab>",      [[<Nop>]],                [[got to last accessed tab]] },
  { "<C-Tab>",     [[<Nop>]],                [[{same as} g<Tab>]] },

  -- [ and ] movement
  { "[[",          [[<Nop>]],                [[go to start of previous section]] },
  { "][",          [[<Nop>]],                [[go to end of next section]] },
  { "[]",          [[<Nop>]],                [[go to end of previous section]] },
  { "]]",          [[<Nop>]],                [[go to start of next section]] },
  { "[`",          [[<Nop>]],                [[go to previous lowercase mark]] },
  { "]`",          [[<Nop>]],                [[go to next lowercase mark]] },
  { "['",          [[<Nop>]],                [[go to previous lowercase mark line]] },
  { "]'",          [[<Nop>]],                [[go to next lowercase mark line]] },
  { '[c',          [[<Nop>]],                [[go to previous change]] },
  { ']c',          [[<Nop>]],                [[go to next change]] },
  { '[f',          [[<Nop>]],                [[{same as} gf]] },
  { ']f',          [[<Nop>]],                [[{same as} gf]] },
  { "[p",          [[<Nop>]],                [[like p but adjust indent to current line]] },
  { "]p",          [[<Nop>]],                [[like p but adjust to cursor under line]] },
  { '[P',          [[<Nop>]],                [[{same as} [p]] },
  { ']P',          [[<Nop>]],                [[{same as} [p]] },
  { "[(",          [[<Nop>]],                [[go to previous unclosed (]] },
  { "])",          [[<Nop>]],                [[go to next unclosed )]] },
  { "[{",          [[<Nop>]],                [[go to previous unclosed {]] },
  { "]}",          [[<Nop>]],                [[go to next unclosed }]] },
  { '[i',          [[<Nop>]],                [[show first line that contains keyword under cursor]] },
  { ']i',          [[<Nop>]],                [[show next line that contains keyword under cursor]] },
  { '[I',          [[<Nop>]],                [[list all lines containing keyword under cursor]] },
  { ']I',          [[<Nop>]],                [[list next lines containing keyword under cursor]] },
  { '[<C-I>',      [[<Nop>]],                [[jump to previous line containing word under cursor]] },
  { ']<C-I>',      [[<Nop>]],                [[jump to next line containing word under cursor]] },
  { "[#",          [[<Nop>]],                [[go to previous unclosed #if or #else]] },
  { "]#",          [[<Nop>]],                [[go to next unclosed #else or #endif]] },
  { "[*",          [[<Nop>]],                [[go to start of /* comment]] },
  { "]*",          [[<Nop>]],                [[go to end of /* comment]] },
  { '[/',          [[<Nop>]],                [[go to start of previous C comment]] },
  { ']/',          [[<Nop>]],                [[go to end of next C comment]] },
  { '[d',          [[<Nop>]],                [[show first #define for keyword under cursor]] },
  { ']d',          [[<Nop>]],                [[show next #define for keyword under cursor]] },
  { '[D',          [[<Nop>]],                [[list all #defines for word under cursor]] },
  { ']D',          [[<Nop>]],                [[list all #defines for word under cursor, searching from cursor position]] },
  { '[<C-D>',      [[<Nop>]],                [[jump to #define for word under cursor]] },
  { ']<C-D>',      [[<Nop>]],                [[jump to #define for word under cursor, searching from cursor position]] },
  { "[m",          [[<Nop>]],                [[go to start of previous method (java)]] },
  { "]m",          [[<Nop>]],                [[go to start of next method (java)]] },
  { "[M",          [[<Nop>]],                [[go to end of previous method (java)]] },
  { "]M",          [[<Nop>]],                [[go to end of next method (java)]] },

  -- insert_mode_mappings

  -- miscellaneous
  { '<C-O>',       [[<Nop>]],                [[execute a single command and return to insert]],                                                                                          mode = [[i]] },
  { '<C-Z>',       [[<Nop>]],                [[when 'insertmode' set: suspend Vim]],                                                                                                     mode = [[i]] },
  { '<C-]>',       [[<Nop>]],                [[trigger abbreviation]],                                                                                                                   mode = [[i]] },

  -- going back to normal mode
  { '<C-C>',       [[<Nop>]],                [[quit insert mode, w/o InsertLeave event]],                                                                                                mode = [[i]] },
  { '<C-L>',       [[<Nop>]],                [[when 'insertmode' set: Leave Insert mode]],                                                                                               mode = [[i]] },

  -- inserting
  { '<C-@>',       [[<Nop>]],                [[insert previously inserted text and stop]],                                                                                               mode = [[i]] },
  { '<C-A>',       [[<Nop>]],                [[insert previously inserted text]],                                                                                                        mode = [[i]] },
  { '<C-R>',       [[<Nop>]],                [[insert the contents of a register]],                                                                                                      mode = [[i]] },
  { '<C-E>',       [[<Nop>]],                [[insert the character which is below the cursor]],                                                                                         mode = [[i]] },
  { '<C-Y>',       [[<Nop>]],                [[insert the character which is above the cursor]],                                                                                         mode = [[i]] },
  { '<C-Q>',       [[<Nop>]],                [[same as CTRL-V, unless used for terminal]],                                                                                               mode = [[i]] },
  { '<C-V>',       [[<Nop>]],                [[insert next non-digit literally]],                                                                                                        mode = [[i]] },

  -- deleting
  { '<C-U>',       [[<Nop>]],                [[delete all entered characters in the current]],                                                                                           mode = [[i]] },

  -- indenting
  { '<C-T>',       [[<Nop>]],                [[insert one shiftwidth of indent in current]],                                                                                             mode = [[i]] },
  { '<C-D>',       [[<Nop>]],                [[delete one shiftwidth of indent in the current]],                                                                                         mode = [[i]] },

  -- duplicates of named keys
  --{ '<C-[>',       [[<Nop>]],            [[same as <Esc>]],                                                                                                                          mode = [[i]] },
  { '<C-M>',       [[<Nop>]],                [[same as <CR>]],                                                                                                                           mode = [[i]] },
  { '<C-H>',       [[<Nop>]],                [[same as <BS>]],                                                                                                                           mode = [[i]] },
  { '<C-I>',       [[<Nop>]],                [[same as <Tab>]],                                                                                                                          mode = [[i]] },
  { '<C-J>',       [[<Nop>]],                [[same as <CR>]],                                                                                                                           mode = [[i]] },

  -- :lmap stuff
  { '<C-^>',       [[<Nop>]],                [[toggle use of |:lmap| mappings]],                                                                                                         mode = [[i]] },
  { '<C-_>',       [[<Nop>]],                [[switch between languages]],                                                                                                               mode = [[i]] },

  -- ctrl-g mappings
  { '<C-G>j',      [[<Nop>]],                [[line down, to column where inserting started line down, to column where inserting started line down, to column where inserting started]], mode = [[i]] },
  { '<C-G>k',      [[<Nop>]],                [[line up, to column where inserting started line up, to column where inserting started line up, to column where inserting started]],       mode = [[i]] },
  { '<C-G>u',      [[<Nop>]],                [[start new undoable edit]],                                                                                                                mode = [[i]] },
  { '<C-G>U',      [[<Nop>]],                [[don't break undo with next cursor movement]],                                                                                             mode = [[i]] },

  -- completion
  { '<C-N>',       [[<Nop>]],                [[complete, next match]],                                                                                                                   mode = [[i]] },
  { '<C-P>',       [[<Nop>]],                [[complete, previous match]],                                                                                                               mode = [[i]] },
  { '<C-X><C-D>',  [[<Nop>]],                [[complete defined identifiers]],                                                                                                           mode = [[i]] },
  { '<C-X><C-E>',  [[<Nop>]],                [[scroll up]],                                                                                                                              mode = [[i]] },
  { '<C-X><C-F>',  [[<Nop>]],                [[complete file names]],                                                                                                                    mode = [[i]] },
  { '<C-X><C-I>',  [[<Nop>]],                [[complete identifiers]],                                                                                                                   mode = [[i]] },
  { '<C-X><C-K>',  [[<Nop>]],                [[complete identifiers from dictionary]],                                                                                                   mode = [[i]] },
  { '<C-X><C-L>',  [[<Nop>]],                [[complete whole lines]],                                                                                                                   mode = [[i]] },
  { '<C-X><C-N>',  [[<Nop>]],                [[next completion]],                                                                                                                        mode = [[i]] },
  { '<C-X><C-O>',  [[<Nop>]],                [[omni completion]],                                                                                                                        mode = [[i]] },
  { '<C-X><C-P>',  [[<Nop>]],                [[previous completion]],                                                                                                                    mode = [[i]] },
  { '<C-X><C-S>',  [[<Nop>]],                [[spelling suggestions]],                                                                                                                   mode = [[i]] },
  { '<C-X><C-T>',  [[<Nop>]],                [[complete identifiers from thesaurus]],                                                                                                    mode = [[i]] },
  { '<C-X><C-Y>',  [[<Nop>]],                [[scroll down]],                                                                                                                            mode = [[i]] },
  { '<C-X><C-U>',  [[<Nop>]],                [[complete with 'completefunc']],                                                                                                           mode = [[i]] },
  { '<C-X><C-V>',  [[<Nop>]],                [[complete like in : command line]],                                                                                                        mode = [[i]] },
  { '<C-X><C-Z>',  [[<Nop>]],                [[stop completion, keeping the text as-is]],                                                                                                mode = [[i]] },
  { '<C-X><C-]>',  [[<Nop>]],                [[complete tags]],                                                                                                                          mode = [[i]] },
  { '<C-X>s',      [[<Nop>]],                [[spelling suggestions]],                                                                                                                   mode = [[i]] }
}

local presets = {
  { [[@undo_break_point]], with = undo_break_point, mode = [[i]] }
}

function M.setup()
  key.load(anchors):map(presets):collect_and_set()
end

return M
