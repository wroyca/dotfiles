local M = {}
local key = require [[detail.key]]

local function undo_break_point(src)
  local key = src[1]
  return key .. [[<C-g>u]]
end

local anchors = {
  { true,        [[@enhanced.j]] },
  { true,        [[@enhanced.k]] },
  { true,        [[@enhanced.n]] },
  { true,        [[@enhanced.N]] },
  { true,        [[@enhanced.shl]] },
  { true,        [[@enhanced.shr]] },
  { true,        [[@enhanced.esc]] },

  { [[,]],       [[@undo_break_point]] },
  { [[.]],       [[@undo_break_point]] },
  { [[;]],       [[@undo_break_point]] },
  { [[{]],       [[@undo_break_point]] },
  { [[}]],       [[@undo_break_point]] },
  { [["]],       [[@undo_break_point]] },
  { [[']],       [[@undo_break_point]] },
  { [[<]],       [[@undo_break_point]] },
  { [[>]],       [[@undo_break_point]] },
  { "[",         [[@undo_break_point]] },
  { "]",         [[@undo_break_point]] },
  { "<space>",   [[@undo_break_point]] },
  { "<cr>",      [[@undo_break_point]] },
  { [[<CR>]],    [[@cmp.abort]] },

  { [[<S-CR>]],  [[@cmp.complete]] },
  { [[<Tab>]],   [[@cmp.confirm_insert]] },
  { [[<S-Tab>]], [[@cmp.confirm_replace]] },
--  { [[<M-p>]],   [[@cmp.snip_previous]],   mode = { [[n]], [[x]], [[v]] } },
--  { [[<M-n>]],   [[@cmp.snip_next]],       mode = { [[n]], [[x]], [[v]] } },
  { [[<C-p>]],   [[@cmp.select_prev_item]] },
  { [[<C-n>]],   [[@cmp.select_next_item]] },
  { [[<S-p>]],   [[@cmp.scroll_docs_up]] },
  { [[<S-n>]],   [[@cmp.scroll_docs_down]] },
}

local presets = {
  { [[@enhanced.j]],       "v:count == 0 ? 'gj' : 'j'", key = [[j]],     mode = { [[n]], [[x]] },        expr = true },
  { [[@enhanced.k]],       "v:count == 0 ? 'gk' : 'k'", key = [[k]],     mode = { [[n]], [[x]] },        expr = true },
  { [[@enhanced.n]],       "'Nn'[v:searchforward]",     key = [[n]],     mode = { [[n]], [[x]], [[o]] }, expr = true, [[Next search]] },
  { [[@enhanced.N]],       "'nN'[v:searchforward]",     key = [[N]],     mode = { [[n]], [[x]], [[o]] }, expr = true, [[Prev search]] },
  { [[@enhanced.shl]],     [[<gv]],                     key = [[<]],     mode = { [[x]] } },
  { [[@enhanced.shr]],     [[>gv]],                     key = [[>]],     mode = { [[x]] } },
  { [[@enhanced.esc]],     [[<cmd>noh<cr><esc>]],       key = [[<esc>]], mode = { [[n]], [[i]] } },
  { [[@undo_break_point]], with = undo_break_point,     mode = [[i]] }
}

function M.setup()
  key.load(anchors):map(presets):collect_and_set()
end

return M
