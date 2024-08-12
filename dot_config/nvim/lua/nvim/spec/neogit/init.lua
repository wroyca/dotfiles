---@type LazyPluginSpec
local Spec = {
  "NeogitOrg/neogit", dependencies = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim", },

  keys = {
    { "<leader>g", "<cmd>Neogit<cr>", desc = "Neogit" },
  },

  config = function(_, opts)
    for _, e in ipairs {
      "commit_editor",
      "commit_select_view",
      "commit_view",
      "log_view",
      "merge_editor",
      "popup",
      "preview_buffer",
      "rebase_editor",
      "reflog_view",
      "tag_editor",
    } do
      opts[e] = { kind = "tab" }
    end
    require("neogit").setup(opts)
  end,
}

return Spec
