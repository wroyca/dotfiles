return {
  [[lewis6991/gitsigns.nvim]],
  event = [[VeryLazy]],

  opts = function()
    return {
      preview_config = {
        border = [[single]]
      },

      signs = {
        add = {
          text = [[┃]]
        },
        untracked = {
          text = [[]]
        },
        change = {
          text = [[┃]]
        },
        delete = {
          text = [[┃]]
        },
        topdelete = {
          text = [[┃]]
        },
        changedelete = {
          text = [[┃]]
        }
      },

      current_line_blame = false,
      current_line_blame_opts = {
        virt_text = true,
        virt_text_pos = [[eol]],
        delay = 100
      },

      diff_opts = {
        algorithm = [[histogram]],
        internal = true,
        indent_heuristic = true
      }
    }
  end
}
