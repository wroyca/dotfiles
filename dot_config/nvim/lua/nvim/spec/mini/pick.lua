---@type LazyPluginSpec
return {
  "mini.pick", dev = true,

  keys = function ()
    local pick = require "mini.pick"
    local extra = require "mini.extra"
    return {
      { "<leader>pb",   pick.builtin.buffers,                                                              desc = "Buffers"        },
      { "<leader>pc",   pick.builtin.cli,                                                                  desc = "Cli"            },
      { "<leader>pf",   pick.builtin.files,                                                                desc = "Files"          },
      { "<leader>pg",   pick.builtin.grep,                                                                 desc = "Grep"           },
      { "<leader>pG",   pick.builtin.grep_live,                                                            desc = "Grep Live"      },
      { "<leader>ph",   pick.builtin.help,                                                                 desc = "Help"           },
      { "<leader>pr",   pick.builtin.resume,                                                               desc = "Resume"         },

      -- Extra

      { "<leader>peb",  extra.pickers.buf_lines,                                                           desc = "Buf lines"      },
      { "<leader>pec",  extra.pickers.commands,                                                            desc = "Commands"       },
      { "<leader>ped",  extra.pickers.diagnostic,                                                          desc = "Diagnostic"     },
      { "<leader>pee",  extra.pickers.explorer,                                                            desc = "Explorer"       },
      { "<leader>pegb", extra.pickers.git_branches,                                                        desc = "Git branches"   },
      { "<leader>pegc", extra.pickers.git_commits,                                                         desc = "Git commits"    },
      { "<leader>pegf", extra.pickers.git_files,                                                           desc = "Git files"      },
      { "<leader>pegh", extra.pickers.git_hunks,                                                           desc = "Git hunks"      },
      { "<leader>pehp", extra.pickers.hipatterns,                                                          desc = "Patterns"       },
      { "<leader>peH",  extra.pickers.history,                                                             desc = "History"        },
      { "<leader>pehg", extra.pickers.hl_groups,                                                           desc = "Groups"         },
      { "<leader>pek",  extra.pickers.keymaps,                                                             desc = "Keymaps"        },
      { "<leader>peL",  extra.pickers.lsp,                                                                 desc = "Lsp"            },
      { "<leader>pem",  extra.pickers.marks,                                                               desc = "Marks"          },
      { "<leader>peo",  extra.pickers.oldfiles,                                                            desc = "Oldfiles"       },
      { "<leader>peO",  extra.pickers.options,                                                             desc = "Options"        },
      { "<leader>per",  extra.pickers.registers,                                                           desc = "Registers"      },
      { "<leader>pes",  extra.pickers.spellsuggests,                                                       desc = "Spell suggests" },
      { "<leader>pet",  extra.pickers.treesitter,                                                          desc = "Treesitter"     },
      { "<leader>pevp", extra.pickers.visit_paths,                                                         desc = "Path"           },
      { "<leader>pevl", extra.pickers.visit_labels,                                                        desc = "Label"          },

      -- Extra (scope)

      { "<leader>pelq", function() require "mini.extra".pickers.list({ scope = "quickfix"      }) end,     desc = "Quickfix"       },
      { "<leader>pell", function() require "mini.extra".pickers.list({ scope = "location-list" }) end,     desc = "Location"       },
      { "<leader>pelj", function() require "mini.extra".pickers.list({ scope = "jumplist"      }) end,     desc = "Jump"           },
      { "<leader>pelc", function() require "mini.extra".pickers.list({ scope = "changelist"    }) end,     desc = "Change"         },
    }
  end,

  config = function(_, opts)
    local pick = require "mini.pick"

    vim.ui.select = pick.ui_select

    pick.setup(opts)
  end
}

