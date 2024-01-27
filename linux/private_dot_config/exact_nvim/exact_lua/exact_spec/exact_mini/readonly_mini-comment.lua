---@type LazyPluginSpec
return {
  {
    [[mini.comment]],
    enabled = false -- https://www.reddit.com/r/neovim/comments/yur29i/comment/j723ch5
  },
  {
    [[numToStr/Comment.nvim]],
    name = [[comment]],
    dependencies = { { name = [[treesitter-context-commentstring]], [[JoosepAlviste/nvim-ts-context-commentstring]] } },
    keys = {
      { [[gc]], mode = { [[n]], [[v]] }, desc = [[Toggle comment linewise]]  },
      { [[gb]], mode = { [[n]], [[v]] }, desc = [[Toggle comment blockwise]] }
    },
    opts = function(_, opts)
      opts.pre_hook =
        require [[ts_context_commentstring.integrations.comment_nvim]].create_pre_hook()
    end
  }
}
