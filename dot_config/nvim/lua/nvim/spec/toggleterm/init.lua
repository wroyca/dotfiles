---@module "toggleterm"

local Spec = {
  "akinsho/toggleterm.nvim",

  keys = function()
    local terminal = require("toggleterm.terminal").Terminal
    local magit = terminal:new({
      cmd = "emacs -nw --init-directory=~/.config/nvim/elisp --magit .",
      autochdir = true,
      direction = "float",
    })

    return {
      { "<leader>g", function() magit:toggle() end, desc = "Magit" }
    }
  end,

  opts = {
    autochdir = true
  },

  config = function(_, opts)
    require ("toggleterm").setup(opts)
  end
}

function _G.set_terminal_keymaps()
  local opts = { buffer = 0 }
  vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)
  vim.keymap.set("t", "jk", [[<C-\><C-n>]], opts)
  vim.keymap.set("t", "<C-h>", [[<Cmd>wincmd h<CR>]], opts)
  vim.keymap.set("t", "<C-j>", [[<Cmd>wincmd j<CR>]], opts)
  vim.keymap.set("t", "<C-k>", [[<Cmd>wincmd k<CR>]], opts)
  vim.keymap.set("t", "<C-l>", [[<Cmd>wincmd l<CR>]], opts)
  vim.keymap.set("t", "<C-w>", [[<C-\><C-n><C-w>]], opts)
end

vim.cmd "autocmd! TermOpen term://*toggleterm#* lua set_terminal_keymaps()"
local restore_terminal_mode = vim.api.nvim_create_augroup("restore_terminal_mode", { clear = true })
vim.api.nvim_create_autocmd({ "TermEnter", "TermLeave" }, {
	pattern = "term://*",
	callback = function()
    vim.cmd.startinsert()
	end,
	group = restore_terminal_mode,
})

vim.api.nvim_create_autocmd("BufEnter", {
	pattern = "term://*",
	callback = function()
			vim.cmd.startinsert()
	end,
	group = restore_terminal_mode,
})

return Spec
