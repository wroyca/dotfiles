local Spec = {
  { "willothy/flatten.nvim", config = true, lazy = false, priority = 1001 },

  {
    "magit", virtual = true, lazy = false,

    keys = {
      {
        "<leader>g",
        function ()
          require ("magit").toggle ()
        end,
        desc = "Magit",
      },
    },

    config = function ()
      require ("magit").setup ({})
    end,
  },
}

return Spec
