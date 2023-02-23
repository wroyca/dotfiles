return {
 "lourenci/github-colors",
  lazy = false,
  priority = 1000, -- make sure to load this before all the other start plugins
  config = function()
	  vim.cmd[[
	    colorscheme     github-colors 
	    hi Normal       guibg=#0a0a0a
	    hi CursorColumn guibg=#0a0a0a
	    hi WildMenu     guibg=#0a0a0a
	    hi Pmenu        guibg=#0a0a0a
	    hi PmenuSel     guibg=#0a0a0a
	    hi PmenuSbar    guibg=#0a0a0a
	    hi PmenuThumb   guibg=#adbac7
	    hi SignColumn   guibg=#0a0a0a
	  ]]
  end
}
