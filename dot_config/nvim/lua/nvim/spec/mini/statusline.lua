vim.api.nvim_create_autocmd("User", { pattern = { "CocStatusChange" }, command = "redrawstatus" })

---@type LazyPluginSpec
local Spec = {
  "mini.statusline", dev = true, event = "VimEnter",

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
          ['r']    = { name = 'P',  highlight = 'MiniStatuslineModeOther'   },
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

        MiniStatusline.section_lsp = function (args)
          if MiniStatusline.is_truncated (args.trunc_width) then return "" end

          local filter_duplicates = function(input)
            local dup = {}
            return input:gsub("%S+", function(word)
              if dup[word] then
                return ""
              end
              dup[word] = true
            end)
          end

          local remove_stylua = function(input)
            return input:gsub("stylua", "%%")
          end

          local truncate_after_whitespace = function(input)
            return input:match("^(.-%s%s)")
          end

          local is_single_word = function(input)
            return input:match("^%S+$") ~= nil
          end

          local sanitize = function(input)
            local output = filter_duplicates(input)
            output = remove_stylua(output)
            return truncate_after_whitespace(output)
          end

          return vim.g.coc_status and string.format("%s", sanitize(vim.g.coc_status) or "") or ""
        end

        local mode          = MiniStatusline.section_mode()
        local git           = MiniStatusline.section_git({ trunc_width = 40 })
        local diff          = MiniStatusline.section_diff({ trunc_width = 75 })
        local diagnostics   = MiniStatusline.section_diagnostics({ trunc_width = 75 })
        local lsp           = MiniStatusline.section_lsp({ trunc_width = 75 })
        local filename      = MiniStatusline.section_filename()
        local fileinfo      = MiniStatusline.section_fileinfo()
        local location      = MiniStatusline.section_location({ trunc_width = 75 })

        return MiniStatusline.combine_groups({
          { hl = mode.highlight,           strings = { mode.name } },
          { hl = 'MiniStatuslineDevinfo',  strings = { git, diff } },
          '%<', -- Mark general truncate point
          { hl = 'MiniStatuslineFilename', strings = { filename } },
          '%=', -- End left alignment
          { hl = 'MiniStatuslineFilename',  strings = { diagnostics, lsp } },
          { hl = 'MiniStatuslineFilename', strings = { fileinfo } },
          { hl = mode.highlight,           strings = { location } },
        })
      end
    }
  },
}

return Spec
