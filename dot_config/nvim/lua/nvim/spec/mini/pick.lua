---@module "mini.pick"

--- Pick from local help tags
---
--- Notes:
--- - On choose executes |:help| command with appropriate modifier
---   (|:horizontal|, |:vertical|, |:tab|) due to the effect of custom mappings.
---
---@param local_opts __pick_builtin_local_opts
---   Not used at the moment.
---@param opts __pick_builtin_opts
local help = function(local_opts, opts)
  local help_dir = vim.fn.expand "~/.config/nvim/doc/"

  -- Get all tags
  local help_buf = vim.api.nvim_create_buf(false, true)
  vim.bo[help_buf].buftype = "help"
  local tags = vim.api.nvim_buf_call(help_buf, function() return vim.fn.taglist ".*" end)
  vim.api.nvim_buf_delete(help_buf, { force = true })
  tags = vim.tbl_filter(function(t) return vim.fn.fnamemodify(t.filename, ":p"):sub(1, #help_dir) == help_dir end, tags)
  vim.tbl_map(function(t) t.text = t.name end, tags)

  -- NOTE: Choosing is done on next event loop to properly overcome special
  -- nature of `:help {subject}` command. For example, it didn't quite work
  -- when choosing tags in same file consecutively.
  local choose = function(item, modifier)
    if item == nil then return end
    vim.schedule(function() vim.cmd((modifier or "") .. "help " .. (item.name or "")) end)
  end
  local preview = function(buf_id, item)
    -- Take advantage of `taglist` output on how to open tag
    vim.api.nvim_buf_call(buf_id, function()
      vim.cmd("noautocmd edit " .. vim.fn.fnameescape(item.filename))
      vim.bo.buftype, vim.bo.buflisted, vim.bo.bufhidden = "nofile", false, "wipe"
      local has_ts = pcall(vim.treesitter.start, 0)
      if not has_ts then vim.bo.syntax = "help" end

      local cache_hlsearch = vim.v.hlsearch
      -- Make a "very nomagic" search to account for special characters in tag
      local search_cmd = string.gsub(item.cmd, "^/", "/\\V")
      vim.cmd("silent keeppatterns " .. search_cmd)
      -- Here `vim.v` doesn't work: https://github.com/neovim/neovim/issues/25294
      vim.cmd("let v:hlsearch=" .. cache_hlsearch)
      vim.cmd "normal! zt"
    end)
  end

  -- Modify default mappings to work with special `:help` command
  local map_custom = function(char, modifier)
    local f = function()
      choose(MiniPick.get_picker_matches().current, modifier .. " ")
      return true
    end
    return { char = char, func = f }
  end

  --stylua: ignore
  local mappings = {
    choose_in_split   = '', show_help_in_split   = map_custom('<C-s>', ''),
    choose_in_vsplit  = '', show_help_in_vsplit  = map_custom('<C-v>', 'vertical'),
    choose_in_tabpage = '', show_help_in_tabpage = map_custom('<C-t>', 'tab'),
  }

  local source = { items = tags, name = "Help", choose = choose, preview = preview }
  opts = vim.tbl_deep_extend("force", { source = source, mappings = mappings }, opts or {})
  return MiniPick.start(opts)
end

local projects = function(_, opts)
  local projects = {}
  local projects_dir = vim.fn.expand("~/Projects")

  for name, type in vim.fs.dir(projects_dir) do
    if type == "directory" then
      table.insert(projects, { text = name, dir = projects_dir .. '/' .. name })
    end
  end

  local choose = function(item)
    if not item then return end
    vim.fn.chdir(item.dir)

    -- Schedule to focus mini.files, as it doesn't otherwise.
    vim.schedule(function()
      require("mini.files").open(item.dir)
    end)
  end

  local preview = function(buf_id, item)
    if not item then return end

    -- TODO: iterate?
    local files = {}
    local function collect(dir)
      for name, type in vim.fs.dir(dir) do
        local path = dir .. '/' .. name
        if type == "file" and not name:match("^%.") then
          table.insert(files, path:sub(item.dir:len() + 2))
        elseif type == "directory" and not name:match("^%.") then
          collect(path)
        end
      end
    end
    collect(item.dir)

    vim.api.nvim_buf_set_lines(buf_id, 0, -1, false, files)
  end

  local source = { items = projects, name = "Projects", choose = choose, preview = preview }
  opts = vim.tbl_deep_extend("force", { source = source }, opts or {})
  return MiniPick.start(opts)
end

---@type LazyPluginSpec
local Spec = {
  "mini.pick", dev = true,

  keys = function()
    local pick = require "mini.pick"
    local extra = require "mini.extra"

    return {
      { "<leader>pb",   pick.builtin.buffers,                                                                desc = "Buffers"                },
      { "<leader>pc",   pick.builtin.cli,                                                                    desc = "Cli"                    },
      { "<leader>pf",   pick.builtin.files,                                                                  desc = "Files"                  },
      { "<leader>pg",   pick.builtin.grep,                                                                   desc = "Grep"                   },
      { "<leader>pG",   pick.builtin.grep_live,                                                              desc = "Grep Live"              },
      { "<leader>ph",   help,                                                                                desc = "Help"                   },
      { "<leader>pr",   pick.builtin.resume,                                                                 desc = "Resume"                 },
      { "<leader>pR",   projects,                                                                            desc = "Projects"               },

      -- Extra

      { "<leader>peb",  extra.pickers.buf_lines,                                                             desc = "Buf Lines"              },
      { "<leader>pec",  extra.pickers.commands,                                                              desc = "Commands"               },
   -- { "<leader>ped",  extra.pickers.diagnostic,                                                            desc = "Diagnostic"             },
      { "<leader>pee",  extra.pickers.explorer,                                                              desc = "Explorer"               },
      { "<leader>pegb", extra.pickers.git_branches,                                                          desc = "Git branches"           },
      { "<leader>pegc", extra.pickers.git_commits,                                                           desc = "Git commits"            },
      { "<leader>pegf", extra.pickers.git_files,                                                             desc = "Git files"              },
      { "<leader>pegh", extra.pickers.git_hunks,                                                             desc = "Git hunks"              },
      { "<leader>pehp", extra.pickers.hipatterns,                                                            desc = "Patterns"               },
      { "<leader>peH",  extra.pickers.history,                                                               desc = "History"                },
      { "<leader>pehg", extra.pickers.hl_groups,                                                             desc = "Groups"                 },
      { "<leader>pek",  extra.pickers.keymaps,                                                               desc = "Keymaps"                },
   -- { "<leader>peL",  extra.pickers.lsp,                                                                 desc = [[Lsp]]                  },
      { "<leader>pem",  extra.pickers.marks,                                                                 desc = "Marks"                  },
      { "<leader>peo",  extra.pickers.oldfiles,                                                              desc = "Oldfiles"               },
      { "<leader>peO",  extra.pickers.options,                                                               desc = "Options"                },
      { "<leader>per",  extra.pickers.registers,                                                             desc = "Registers"              },
      { "<leader>pes",  extra.pickers.spellsuggests,                                                         desc = "Spell suggests"         },
      { "<leader>pet",  extra.pickers.treesitter,                                                            desc = "Treesitter"             },
      { "<leader>pevp", extra.pickers.visit_paths,                                                           desc = "Path"                   },
      { "<leader>pevl", extra.pickers.visit_labels,                                                          desc = "Label"                  },

      -- Extra (scope)

      { "<leader>pelq", function() require "mini.extra".pickers.list({ scope = "quickfix"      }) end,       desc = "Quickfix"               },
      { "<leader>pell", function() require "mini.extra".pickers.list({ scope = "location-list" }) end,       desc = "Location"               },
      { "<leader>pelj", function() require "mini.extra".pickers.list({ scope = "jumplist"      }) end,       desc = "Jump"                   },
      { "<leader>pelc", function() require "mini.extra".pickers.list({ scope = "change"        }) end,       desc = "Change"                 },
    }
  end,

  config = function(_, opts)
    local pick = require [[mini.pick]]

    vim.ui.select = pick.ui_select


    pick.setup(opts)
  end
}

return Spec
