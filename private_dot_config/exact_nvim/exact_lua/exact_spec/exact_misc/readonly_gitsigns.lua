local function revision() return vim.fn.input([[(gitsigns) Revision {base}: ]]) end
local function bufnr() return vim.fn.input([[(gitsigns) Buffer {bufnr}: ]]) end
local function reset_buffer_index()
  local choice = vim.fn.confirm(("(gitsigns) this runs an `git reset` on current buffers file. Continue?"),
    "&Yes\n&No\n&Cancel", 1, "Question")
  if choice == 1 then -- Yes
    require [[gitsigns]].reset_buffer_index()
  end
end

return {
  [[lewis6991/gitsigns.nvim]],
  name = [[gitsigns]],
  event = [[VeryLazy]],
  keys = {
    { [[<leader>ghts]],  function() require [[gitsigns]].toggle_signs() end,              desc = [[Signs]] },
    { [[<leader>ghthn]], function() require [[gitsigns]].toggle_numhl() end,              desc = [[Number]] },
    { [[<leader>ghthl]], function() require [[gitsigns]].toggle_linehl() end,             desc = [[Line]] },
    { [[<leader>ghtw]],  function() require [[gitsigns]].toggle_word_diff() end,          desc = [[Word diff]] },
    { [[<leader>ghtw]],  function() require [[gitsigns]].toggle_deleted() end,            desc = [[Deleted]] },
    { [[<leader>ghtb]],  function() require [[gitsigns]].toggle_current_line_blame() end, desc = [[Inlay]] },
    { [[<leader>ghPl]],  function() require [[gitsigns]].setloclist() end,                desc = [[Location]] },
    { [[<leader>ghPq]],  function() require [[gitsigns]].setqflist() end,                 desc = [[Quickfix]] },
    { [[<leader>ghs]],   function() require [[gitsigns]].show(revision()) end,            desc = [[Show]] },
    { [[<leader>ghd]],   function() require [[gitsigns]].diffthis(revision()) end,        desc = [[Diff]] },
    { [[<leader>ghbrb]], function() require [[gitsigns]].reset_base() end,                desc = [[Base]] },
    { [[<leader>ghc]],   function() require [[gitsigns]].change_base(revision()) end,     desc = [[Change]] },
    { [[<leader>ghlb]],  function() require [[gitsigns]].blame_line() end,                desc = [[Blame]] },
    { [[<leader>ghg]],   function() require [[gitsigns]].get_hunks(bufnr()) end,          desc = [[Get]] },
    { [[<leader>ghS]],   function() require [[gitsigns]].select_hunk() end,               desc = [[Select]] },
    { [[<leader>ghpi]],  function() require [[gitsigns]].preview_hunk_inline() end,       desc = [[Inline]] },
    { [[<leader>ghpp]],  function() require [[gitsigns]].preview_hunk() end,              desc = [[Float]] },
    { [[<leader>ghjn]],  function() require [[gitsigns]].next_hunk() end,                 desc = [[Next]] },
    { [[<leader>ghjp]],  function() require [[gitsigns]].prev_hunk() end,                 desc = [[Previous]] },
    { [[<leader>ghbri]], function() reset_buffer_index() end,                             desc = [[Index]] },
    { [[<leader>ghbs]],  function() require [[gitsigns]].stage_buffer() end,              desc = [[Stage]] },
    { [[<leader>ghu]],   function() require [[gitsigns]].undo_stage_hunk() end,           desc = [[Undo stage]] },
    { [[<leader>ghbra]], function() require [[gitsigns]].reset_buffer() end,              desc = [[All]] },
    { [[<leader>ghlr]],  function() require [[gitsigns]].reset_hunk() end,                desc = [[Reset]] },
    { [[<leader>ghls]],  function() require [[gitsigns]].stage_hunk() end,                desc = [[Stage]] },
  },

  opts = function()
    return {
      signs = {
        add          = { text = [[┃]] },
        untracked    = { text = [[ ]] },
        change       = { text = [[┃]] },
        delete       = { text = [[┃]] },
        topdelete    = { text = [[┃]] },
        changedelete = { text = [[┃]] }
      },

      diff_opts = {
        algorithm = [[histogram]],
        internal = true,
        indent_heuristic = true
      }
    }
  end
}
