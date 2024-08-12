---@module "mini.statusline"
---@diagnostic disable: duplicate-set-field

---@type LazyPluginSpec
local Spec = {
  "mini.statusline", dev = true, event = "VimEnter", enabled = false,

  init = function ()
    vim.o.laststatus = 3
  end,

  opts = {
    set_vim_settings = false,
    content = {
      active = function ()
        local CTRL_S = vim.api.nvim_replace_termcodes('<C-S>', true, true, true)
        local CTRL_V = vim.api.nvim_replace_termcodes('<C-V>', true, true, true)

        local modes = setmetatable({
          ['n']    = { name = 'NO', highlight = 'MiniStatuslineModeNormal'  },
          ['v']    = { name = 'VI', highlight = 'MiniStatuslineModeVisual'  },
          ['V']    = { name = 'VL', highlight = 'MiniStatuslineModeVisual'  },
          [CTRL_V] = { name = 'VB', highlight = 'MiniStatuslineModeVisual'  },
          ['s']    = { name = 'SE', highlight = 'MiniStatuslineModeVisual'  },
          ['S']    = { name = 'SL', highlight = 'MiniStatuslineModeVisual'  },
          [CTRL_S] = { name = 'SB', highlight = 'MiniStatuslineModeVisual'  },
          ['i']    = { name = 'IN', highlight = 'MiniStatuslineModeInsert'  },
          ['R']    = { name = 'RE', highlight = 'MiniStatuslineModeReplace' },
          ['c']    = { name = 'CO', highlight = 'MiniStatuslineModeCommand' },
          ['r']    = { name = 'PR', highlight = 'MiniStatuslineModeOther'   },
          ['!']    = { name = 'SH', highlight = 'MiniStatuslineModeOther'   },
          ['t']    = { name = 'TE', highlight = 'MiniStatuslineModeOther'   },
        }, {
          -- By default return 'Unknown' but this shouldn't be needed
          __index = function()
            return { name = 'UN', highlight = '%#MiniStatuslineModeOther#' }
          end,
        })

        MiniStatusline.section_mode = function()
          return modes[vim.fn.mode()]
        end

        MiniStatusline.section_filename = function ()
          return vim.bo.buftype == "terminal" and "^t" or "%f%m%r"
        end

        MiniStatusline.section_fileinfo = function()
          local get_filesize = function ()
            local size = vim.fn.getfsize (vim.fn.getreg  "%")
            if size < 0 then return "" end
            if size < 1024 then
              return string.format ("%dB", size)
            elseif size < 1048576 then
              return string.format ("%.2fKiB", size / 1024)
            else
              return string.format ("%.2fMiB", size / 1048576)
            end
          end
          return vim.bo.filetype == '' and '' or string.format ("%s", get_filesize ())
        end

        MiniStatusline.section_location = function(args)
          if MiniStatusline.is_truncated(args.trunc_width) then return ' %02v ' end
          return ' %02v:%02{max([1, virtcol("$") - 1])} '
        end

        local set_default_hl = function(name, data)
          data.default = true
          vim.api.nvim_set_hl(0, name, data)
        end

        set_default_hl('MiniStatuslineDiagnosticError', { link = 'DiagnosticError' })
        set_default_hl('MiniStatuslineDiagnosticHint',  { link = 'DiagnosticHint' })
        set_default_hl('MiniStatuslineDiagnosticInfo',  { link = 'DiagnosticInfo' })
        set_default_hl('MiniStatuslineDiagnosticWarn',  { link = 'DiagnosticWarn' })

        local severities = {
          error = { symbol = "E", hl_group = "MiniStatuslineDiagnosticError" },
          hint = { symbol = "H", hl_group = "MiniStatuslineDiagnosticHint" },
          information = { symbol = "I", hl_group = "MiniStatuslineDiagnosticInfo" },
          warning = { symbol = "W", hl_group = "MiniStatuslineDiagnosticWarn" },
        }

        MiniStatusline.section_diagnostics = function(args)
          if MiniStatusline.is_truncated(args.trunc_width) then return '' end

          local count = vim.b.coc_diagnostic_info or {}
          local diagnostics_list = {}
          for severity, info in pairs(severities) do
            local n = count[severity] or 0
            if n > 0 then
              table.insert(diagnostics_list, ("%%#%s#%s%d"):format(info.hl_group, info.symbol, n))
            end
          end
          if #diagnostics_list == 0 then return '' end

          return table.concat(diagnostics_list, " ")
        end

        local mode        = MiniStatusline.section_mode()
        local git         = MiniStatusline.section_git({ trunc_width = 40 })
        local diff        = MiniStatusline.section_diff({ trunc_width = 75 })
        local diagnostics = MiniStatusline.section_diagnostics({ trunc_width = 75 })
        local filename    = MiniStatusline.section_filename()
        local fileinfo    = MiniStatusline.section_fileinfo()
        local location    = MiniStatusline.section_location({ trunc_width = 75 })

        return MiniStatusline.combine_groups({
          { hl = mode.highlight,            strings = { mode.name } },
          { hl = 'MiniStatuslineDevinfo',   strings = { git, diff } },
          '%<',
          { hl = 'MiniStatuslineFilename',  strings = { filename } },
          '%=',
          { hl = 'MiniStatuslineFilename',  strings = { diagnostics } },
          { hl = 'MiniStatuslineFilename',  strings = { fileinfo } },
          { hl = mode.highlight,            strings = { location } },
        })
      end
    }
  }
}

return Spec
