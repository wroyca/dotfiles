---@type LazyPluginSpec
return {
  [[echasnovski/mini.comment]],
  event = [[VeryLazy]],

  opts = {
    options = {
      custom_commentstring = function()
        local has_ts, ts = pcall(
          require, [[ts_context_commentstring.internal]]
        )
        return has_ts and ts.calculate_commentstring() or vim.bo.commentstring
      end
    }
  }
}
