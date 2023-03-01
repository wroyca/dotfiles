return {
  {
   "sainnhe/sonokai",
    config = function()
      vim.g.sonokai_disable_italic_comment = 1
      vim.g.sonokai_enable_italic = 0
      vim.g.sonokai_cursor = "green"
      vim.g.sonokai_transparent_background = 1
      vim.g.sonokai_menu_selection_background = "green"
      vim.g.sonokai_show_eob = 0
      vim.g.sonokai_diagnostic_text_highlight = 1
      vim.g.sonokai_diagnostic_line_highlight = 0
      vim.g.sonokai_diagnostic_virtual_text = "colored"
      vim.cmd[[
        colorscheme sonokai
        highlight Normal guibg=black
        highlight EndOfBuffer guibg=black guifg=black
        highlight NormalFloat ctermbg=NONE guibg=NONE
        highlight FloatTitle ctermbg=NONE guibg=NONE
        highlight FloatBorder ctermbg=NONE guibg=NONE
      ]]
    end
  }
}
