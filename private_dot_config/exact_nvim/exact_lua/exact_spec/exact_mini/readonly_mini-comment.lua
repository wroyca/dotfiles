---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[echasnovski/mini.comment]],
    enabled = false
  },
  {
    [[numToStr/Comment.nvim]],
    keys = {
      { [[gc]], mode = { [[n]], [[v]] }, desc = [[Comment toggle linewise]] },
      { [[gb]], mode = { [[n]], [[v]] }, desc = [[Comment toggle blockwise]] },
    },
    opts = function(_, opts)
      local has_cs, cs = pcall(require, [[ts_context_commentstring.integrations.comment_nvim]])
      if has_cs then opts.pre_hook = cs.create_pre_hook() end
    end
  }
}
