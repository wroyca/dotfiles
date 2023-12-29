return {
  [[luukvbaal/statuscol.nvim]],
  name = [[statuscol]],
  event = [[VeryLazy]],

  config = function()
    local statuscol = require [[statuscol]]
    statuscol.setup({
      ft_ignore = { [[help]], [[mason]], [[noice]], [[lazy]] },

      -- Default segments (gitsign -> separator)
      segments = {
        { sign = { namespace = { [[gitsign]] } }, click = [[v:lua.ScSa]] },
        { text = { [[ ]] },                       click = [[v:lua.ScLa]] }
      }
    })
  end
}
