---@type LazyPluginSpec
return {
  [[mini.clue]], event = [[VeryLazy]],

  opts = function()
    local miniclue = require [[mini.clue]]
    return {
      triggers = {
        -- Leader triggers
        { mode = [[n]], keys = [[<Leader>]] },
        { mode = [[x]], keys = [[<Leader>]] },

        -- Built-in completion
        { mode = [[i]], keys = [[<C-x>]] },

        -- `g` key
        { mode = [[n]], keys = [[g]] },
        { mode = [[x]], keys = [[g]] },

        -- Marks
        { mode = [[n]], keys = [[']] },
        { mode = [[n]], keys = [[`]] },
        { mode = [[x]], keys = [[']] },
        { mode = [[x]], keys = [[`]] },

        -- Registers
        { mode = [[n]], keys = [["]] },
        { mode = [[x]], keys = [["]] },
        { mode = [[i]], keys = [[<C-r>]] },
        { mode = [[c]], keys = [[<C-r>]] },

        -- Window commands
        { mode = [[n]], keys = [[<C-w>]] },

        -- `z` key
        { mode = [[n]], keys = [[z]] },
        { mode = [[x]], keys = [[z]] },
      },

      -- Add descriptions for mapping groups
      clues = {
        miniclue.gen_clues.builtin_completion(),
        miniclue.gen_clues.g(),
        miniclue.gen_clues.marks(),
        miniclue.gen_clues.registers(),
        miniclue.gen_clues.windows({
          submode_move = true,
          submode_navigate = true,
          submode_resize = true,
        }),
        miniclue.gen_clues.z(),
      },

      window = { config = { width = 55 }, delay = 500 }
    }
  end
}