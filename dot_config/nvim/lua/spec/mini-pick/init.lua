--- 'mini.pick' pickers.
---
--- A table with |MiniPick| pickers (which is a hard dependency).
--- Notes:
--- - All have the same signature:
---     - <local_opts> - optional table with options local to picker.
---     - <opts> - optional table with options forwarded to |MiniPick.start()|.
--- - All of them are automatically registered in |MiniPick.registry|.
--- - All use default versions of |MiniPick-source.preview|, |MiniPick-source.choose|,
---   and |MiniPick-source.choose_marked| if not stated otherwise.
---   Shown text and |MiniPick-source.show| are targeted to the picked items.
---
--- Examples of usage:
--- - As Lua code: `Pickers.foobar()`.
--- - With |:Pick| command: `:Pick foobar scope='current'`
---   Note: this requires calling |Pickers.setup()|.
local Pickers = {}

local pick_error = function(msg)
  error(string.format('(mini.extra) %s', msg), 0)
end

local pick_get_config = function()
  return vim.tbl_deep_extend('force', (require('mini.pick') or {}).config or {}, vim.b.minipick_config or {})
end

local pick_show_with_icons = function(buf_id, items, query)
  require('mini.pick').default_show(buf_id, items, query, { show_icons = true })
end

local pick_validate = function(fun_name)
  local has_pick, pick = pcall(require, 'mini.pick')
  if not has_pick then
    pick_error(string.format([[`pickers.%s()` requires 'mini.pick' which can not be found.]], fun_name))
  end
  return pick
end

local pick_validate_one_of = function(target, opts, values, picker_name)
  if vim.tbl_contains(values, opts[target]) then return opts[target] end
  local msg = string.format(
    '`pickers.%s` has wrong "%s" local option (%s). Should be one of %s.',
    picker_name,
    target,
    vim.inspect(opts[target]),
    table.concat(vim.tbl_map(vim.inspect, values), ', ')
  )
  pick_error(msg)
end

local pick_validate_scope = function(...)
  return pick_validate_one_of('scope', ...)
end

local pick_start = function(items, default_opts, opts)
  local pick = pick_validate()
  local fallback = {
    source = {
      preview = pick.default_preview,
      choose = pick.default_choose,
      choose_marked = pick.default_choose_marked,
    },
  }
  local opts_final = vim.tbl_deep_extend('force', fallback, default_opts, opts or {}, { source = { items = items } })
  return pick.start(opts_final)
end

--- Projects picker
---
--- Pick from existing projects.
---
---@param local_opts __extra_pickers_local_opts
---   Possible fields:
---   - <scope> `(string)` - "all"
---     Default: "all".
---@param opts __extra_pickers_opts
---
---@return __extra_pickers_return
Pickers.projects = function(local_opts, opts)
  local pick = pick_validate('projects')
  local_opts = vim.tbl_deep_extend('force', { scope = 'all' }, local_opts or {})

  local scope = pick_validate_scope(local_opts, { 'all' }, 'projects')
  local is_scope_all = scope == 'all'
  local poke_picker = pick.poke_is_picker_active
  local normalize = function(base, name) return base .. "/" .. name .. "/" end
  local is_git_directory = function(path) return vim.uv.fs_stat(path .. ".git/") end

  -- PERF: Rather than traversing the entire filesystem, let's establish a
  -- single shared projects directory. Also define non-blocking callable `items`
  -- because getting all projects may take visibly long time.
  local p = vim.fn.expand("$HOME") .. "/Projects"
  local f = function()
    local items = {}
    local fs = vim.loop.fs_scandir(p)
    while true do
      local name, _ = vim.loop.fs_scandir_next(fs)
      if not name then break end
      path = normalize (p, name)
      if is_git_directory(path) then
        table.insert(items, { text = name, path = path })
      end
    end
    pick.set_picker_items(items)
  end

  local items = vim.schedule_wrap(coroutine.wrap(f))
  local show = pick_get_config().source.show
  local choose = function(item)
    print (item.path)
    vim.api.nvim_set_current_dir(item.path)
  end

  return pick_start(items, { source = { name = string.format('Projects', scope), choose = choose } }, opts)
end


return {
  [[mini.pick]], name = [[mini-pick]], main = [[mini.pick]], dev = true,
  keys = function ()
    local pick = require [[mini.pick]]
    local extra = require [[mini.extra]]
    return {
      { [[<leader>pb]],   pick.builtin.buffers,                                                              desc = [[Buffers]]        },
      { [[<leader>pc]],   pick.builtin.cli,                                                                  desc = [[Cli]]            },
      { [[<leader>pf]],   pick.builtin.files,                                                                desc = [[Files]]          },
      { [[<leader>pg]],   pick.builtin.grep,                                                                 desc = [[Grep]]           },
      { [[<leader>pG]],   pick.builtin.grep_live,                                                            desc = [[Grep Live]]      },
      { [[<leader>ph]],   pick.builtin.help,                                                                 desc = [[Help]]           },
      { [[<leader>pr]],   pick.builtin.resume,                                                               desc = [[Resume]]         },
      { [[<leader>pp]],   Pickers.projects,                                                                  desc = [[Projects]]       },

      -- Extra

      { [[<leader>peb]],  extra.pickers.buf_lines,                                                           desc = [[Buf lines]]      },
      { [[<leader>pec]],  extra.pickers.commands,                                                            desc = [[Commands]]       },
      { [[<leader>ped]],  extra.pickers.diagnostic,                                                          desc = [[Diagnostic]]     },
      { [[<leader>pee]],  extra.pickers.explorer,                                                            desc = [[Explorer]]       },
      { [[<leader>pegb]], extra.pickers.git_branches,                                                        desc = [[Git branches]]   },
      { [[<leader>pegc]], extra.pickers.git_commits,                                                         desc = [[Git commits]]    },
      { [[<leader>pegf]], extra.pickers.git_files,                                                           desc = [[Git files]]      },
      { [[<leader>pegh]], extra.pickers.git_hunks,                                                           desc = [[Git hunks]]      },
      { [[<leader>pehp]], extra.pickers.hipatterns,                                                          desc = [[Patterns]]       },
      { [[<leader>peH]],  extra.pickers.history,                                                             desc = [[History]]        },
      { [[<leader>pehg]], extra.pickers.hl_groups,                                                           desc = [[Groups]]         },
      { [[<leader>pek]],  extra.pickers.keymaps,                                                             desc = [[Keymaps]]        },
      { [[<leader>peL]],  extra.pickers.lsp,                                                                 desc = [[Lsp]]            },
      { [[<leader>pem]],  extra.pickers.marks,                                                               desc = [[Marks]]          },
      { [[<leader>peo]],  extra.pickers.oldfiles,                                                            desc = [[Oldfiles]]       },
      { [[<leader>peO]],  extra.pickers.options,                                                             desc = [[Options]]        },
      { [[<leader>per]],  extra.pickers.registers,                                                           desc = [[Registers]]      },
      { [[<leader>pes]],  extra.pickers.spellsuggests,                                                       desc = [[Spell suggests]] },
      { [[<leader>pet]],  extra.pickers.treesitter,                                                          desc = [[Treesitter]]     },
      { [[<leader>pevp]], extra.pickers.visit_paths,                                                         desc = [[Path]]           },
      { [[<leader>pevl]], extra.pickers.visit_labels,                                                        desc = [[Label]]          },

      -- Extra (scope)

      { [[<leader>pelq]], function() require [[mini.extra]].pickers.list({ scope = [[quickfix]]      }) end, desc = [[Quickfix]]       },
      { [[<leader>pell]], function() require [[mini.extra]].pickers.list({ scope = [[location-list]] }) end, desc = [[Location]]       },
      { [[<leader>pelj]], function() require [[mini.extra]].pickers.list({ scope = [[jumplist]]      }) end, desc = [[Jump]]           },
      { [[<leader>pelc]], function() require [[mini.extra]].pickers.list({ scope = [[changelist]]    }) end, desc = [[Change]]         },
    }
  end,

  config = function(_, opts)
    local pick = require [[mini.pick]]

    vim.ui.select = pick.ui_select

    pick.setup(opts)
  end
}
