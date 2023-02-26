local colors = {
  blue   = '#e0e0e0',
  cyan   = '#33b1ff',
  black  = '#161616',
  white  = '#e0e0e0',
  red    = '#ff8389',
  violet = '#78a9ff',
  grey   = '#161616',
}

local clocktheme = {
  normal = {
    a = { fg = colors.black, bg = colors.black },
    b = { fg = colors.black, bg = colors.black },
    c = { fg = colors.white, bg = colors.black },
  },

  insert = { a = { fg = colors.black, bg = colors.black } },
  visual = { a = { fg = colors.black, bg = colors.black } },
  replace = { a = { fg = colors.black, bg = colors.black } },

  inactive = {
    a = { fg = colors.black, bg = colors.black },
    b = { fg = colors.black, bg = colors.black },
    c = { fg = colors.black, bg = colors.black },
  },
}

local clock = 'os.date("%I:%M:%S", os.time())'
if _G.Statusline_timer == nil then
  _G.Statusline_timer = vim.loop.new_timer()
else
  _G.Statusline_timer:stop()
end
_G.Statusline_timer:start(0, 1000, vim.schedule_wrap(
  function()
    vim.api.nvim_command("redrawstatus")
  end)
)

local date = os.date("%d-%m-%Y  %H:%M:%S")
return {
 "nvim-lualine/lualine.nvim",
  lazy = false,
  priority = 1001, -- make sure to load this before all the other start plugins, but after colorscheme.
  config = function()
	  require'lualine'.setup{
	     options = {
        theme = clocktheme,
        component_separators = '',
      },
      sections = {
        lualine_a = {''},
        lualine_b = {''},
        lualine_c = {'%=', clock},
        lualine_x = {''},
        lualine_y = {''},
        lualine_z = {''},
      },
      inactive_sections = {
        lualine_a = {''},
        lualine_b = {''},
        lualine_c = {''},
        lualine_x = {''},
        lualine_y = {''},
        lualine_z = {''},
      },
      tabline = {},
      extensions = {},
	  }
  end
}
