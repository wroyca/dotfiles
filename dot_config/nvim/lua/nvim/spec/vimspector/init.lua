---@module "overseer"

---@type LazyPluginSpec
local Spec = {
  "puremourning/vimspector", event = "VeryLazy",

  init = function()
    vim.o.signcolumn = "yes:1"
  end,
  keys = {
    { "<leader>vl", ":call vimspector#Launch()<CR>",          desc = "Launch"                        },
    { "<leader>vr", ":call vimspector#Reset()<CR>",           desc = "Reset"                         },
    { "<leader>vc", "<Plug>VimspectorContinue",               desc = "Continue"                      },
    --
    { "<leader>vbb", "<Plug>VimspectorToggleBreakpoint",      desc = "Toggle Breakpoint"             },
    { "<leader>vbf", "<Plug>VimspectorAddFunctionBreakpoint", desc = "Toggle Function Breakpoint"    },
    { "<leader>vbc", "<Plug>VimspectorAddFunctionBreakpoint", desc = "Toggle Conditional Breakpoint" },
    --
    { "<leader>vO",  "<Plug>VimspectorStepOut",               desc = "Step Out"                      },
    { "<leader>vi",  "<Plug>VimspectorStepInto",              desc = "Step Into"                     },
    { "<leader>vo",  "<Plug>VimspectorStepOver",              desc = "Step Over"                     },
    --
    { "<leader>vsu", "<Plug>VimspectorUpFrame",               desc = "Navigate Upward"               },
    { "<leader>vsd", "<Plug>VimspectorDownFrame",             desc = "Navigate Downward"             },
    --
    { "<leader>vB",  "<Plug>VimspectorBreakpoints",           desc = "Show Breakpoints"              },
    { "<leader>vD",  "<Plug>VimspectorDisassemble",           desc = "Show Disassembly"              },
    { "<leader>vI",  "<Plug>VimspectorBalloonEval",           desc = "Inspect", mode = { "n", "x" }  },
  },
}

return Spec
