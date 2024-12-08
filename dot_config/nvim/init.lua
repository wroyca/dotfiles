--      .-.      _______                             .  '  *   .  . '
--     {}``; |==|_______D                                  . *  -+-  .
--     / ('        /|\                                 . '   * .    '  *
-- (  /  |        / | \                                    * .  ' .  .-+-
--  \(_)_%s      /  |  \                                *   *  .   .

vim.cmd.packadd ("lumen")

local shada = vim.o.shada
vim.o.shada = ""
vim.schedule (function ()
	vim.o.shada = shada
	pcall (vim.cmd.rshada, { bang = true })
end)

vim.g.mapleader      = vim.keycode ("<Space>")
vim.g.localmapleader = vim.g.mapleader
vim.o.clipboard      = "unnamedplus"
vim.o.guicursor      = "a:blinkwait700-blinkoff400-blinkon250,i-ci-ve:ver25,r-cr-o:hor20"
vim.o.mouse          = "a"
vim.o.mousescroll    = "ver:3,hor:0"
vim.o.mousemoveevent = true
vim.o.confirm        = true
vim.o.list           = true
vim.o.gdefault       = true
vim.o.cursorline     = true
vim.o.scrolloff      = 4
vim.o.pumheight      = 9
vim.o.laststatus     = 3
vim.o.wrap           = false
vim.o.fillchars      = "eob: "
vim.o.breakindent    = true
vim.o.copyindent     = true
vim.o.smartindent    = true
vim.o.preserveindent = true

local function create_autocmds (cmds)
	for _, c in ipairs (cmds) do
		local e, d, cb = unpack (c)
		vim.api.nvim_create_autocmd (e, { desc = d, callback = cb })
	end
end

create_autocmds ({
	{
		{ "InsertLeave", "WinEnter" },
		"Show cursor line in active window",
		function (ev)
			if vim.bo[ev.buf].buftype == "" then
				vim.opt_local.cursorline = true
			end
		end,
	},
	{
		{ "InsertEnter", "WinLeave" },
		"Hide cursor line in inactive windows",
		function ()
			vim.opt_local.cursorline = false
		end,
	},
	{
		{ "VimEnter", "VimResume", "ColorScheme" },
		"Sync terminal background color",
		function ()
			io.stdout:write (string.format ("\027]11;#%06x\007", vim.api.nvim_get_hl (0, { name = "Normal" }).bg))
		end,
	},
	{
		{ "VimLeavePre", "VimSuspend" },
		"Revert terminal background color",
		function ()
			io.stdout:write ("\027]111;;\007")
		end,
	},
})
