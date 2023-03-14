return {
  {
   "akinsho/toggleterm.nvim",
    config = true,
    cmd = "ToggleTerm",
    keys = {
      {
        "<F1>", "<cmd>ToggleTerm<cr>",  desc = "Toggle floating terminal"
      }
    },
    opts = {
      open_mapping = [[<F1>]],
    }
  }
}
