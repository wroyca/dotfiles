---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[mini.comment]],
    dev = true,
    enabled = false -- https://www.reddit.com/r/neovim/comments/yur29i/comment/j723ch5
  },
  {
    [[numToStr/Comment.nvim]],
    name = [[comment]],
    keys = {
      { [[gc]], mode = { [[n]], [[v]] }, desc = [[Toggle comment linewise]] },
      { [[gb]], mode = { [[n]], [[v]] }, desc = [[Toggle comment blockwise]] },
    },
    opts = function(_, opts)
      local cs = require [[ts_context_commentstring.integrations.comment_nvim]]
      opts.pre_hook = cs.create_pre_hook()
    end
  }
}
