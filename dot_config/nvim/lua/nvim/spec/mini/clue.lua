---@type LazyPluginSpec
return {
  "mini.clue", dev = true, event = "VeryLazy",

  init = function()
    vim.cmd.hi "MiniClueBorder cterm=reverse guibg=NONE"
    vim.cmd.hi "MiniClueDescSingle cterm=reverse guibg=NONE"
  end,

  opts = function()
    local miniclue = require "mini.clue"
    return {
      triggers = {
        -- Leader triggers
        { mode = "n", keys = "<Leader>" },
        { mode = "x", keys = "<Leader>" },

        -- Built-in completion
        { mode = "i", keys = "<C-x>" },

        -- `g` key
        { mode = "n", keys = "g" },
        { mode = "x", keys = "g" },

        -- Marks
        { mode = "n", keys = "'" },
        { mode = "n", keys = "`" },
        { mode = "x", keys = "'" },
        { mode = "x", keys = "`" },

        -- Registers
        { mode = "n", keys = '"' },
        { mode = "x", keys = '"' },
        { mode = "i", keys = "<C-r>" },
        { mode = "c", keys = "<C-r>" },

        -- Window commands
        { mode = "n", keys = "<C-w>" },

        -- `z` key
        { mode = "n", keys = "z" },
        { mode = "x", keys = "z" },
      },

      -- Add descriptions for mapping groups
      clues = {
        { mode = "n", keys = "<Leader>b",    desc = "+Buffers"       },
        { mode = "n", keys = "<Leader>bt",   desc = "+Tab"           },
        { mode = "n", keys = "<Leader>l",    desc = "+LSP"           },
        { mode = "n", keys = "<Leader>g",    desc = "+Git"           },
        { mode = "n", keys = "<Leader>p",    desc = "+Pick"          },
        { mode = "n", keys = "<Leader>pe",   desc = "+Extra"         },
        { mode = "n", keys = "<Leader>peg",  desc = "+Git"           },
        { mode = "n", keys = "<Leader>peh",  desc = "+Highlight"     },
        { mode = "n", keys = "<Leader>pel",  desc = "+List"          },
        { mode = "n", keys = "<Leader>pev",  desc = "+Visit"         },
        { mode = "n", keys = "<Leader>v",    desc = "+Visit"         },
      },

      window = { config = { width = 55 }, delay = 500 }
    }
  end
}
