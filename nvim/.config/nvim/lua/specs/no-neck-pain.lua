return {
  {
   "shortcuts/no-neck-pain.nvim",
    keys = {
      {
        "<F2>", "<cmd>NoNeckPain<cr>",  desc = "Toggle centered view"
      }
    },
    config = function()
	    require'no-neck-pain'.setup()
    end
  }
}
