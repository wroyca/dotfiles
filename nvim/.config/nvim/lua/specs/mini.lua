return {
  {
   "echasnovski/mini.nvim",
    event = "InsertEnter",
  },
  {
   "echasnovski/mini.pairs",
    event = "InsertEnter",
    config = function(_, opts)
      require("mini.pairs").setup(opts)
    end
  },
  {
   "echasnovski/mini.trailspace",
    event = "InsertEnter",
    config = function(_, opts)
      require("mini.trailspace").setup(opts)
    end
  }
}

