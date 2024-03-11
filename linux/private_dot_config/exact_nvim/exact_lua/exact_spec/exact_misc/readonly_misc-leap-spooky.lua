---@type LazyPluginSpec
return {
  [[misc-leap-spooky]],
  keys = {
    { [[drr]], mode = [[n]] },
    { [[dRR]], mode = [[n]] },
    { [[dir]], mode = [[n]] },
    { [[dar]], mode = [[n]] },
    { [[diR]], mode = [[n]] },
    { [[daR]], mode = [[n]] },
    { [[yrr]], mode = [[n]] },
    { [[yRR]], mode = [[n]] },
    { [[yir]], mode = [[n]] },
    { [[yar]], mode = [[n]] },
    { [[yiR]], mode = [[n]] },
    { [[yaR]], mode = [[n]] }
  },
  config = function()
    require [[spooky]].setup()
    vim.api.nvim_create_augroup([[SpookyUser]], {})
    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[SpookyOperationDone]],
      group = [[SpookyUser]],
      callback = function(event)
        if vim.v.operator ~= [[c]] then
          event.data.restore_cursor()
        end
      end
    })
  end
}
