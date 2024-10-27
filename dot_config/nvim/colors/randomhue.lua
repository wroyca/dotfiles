local hues = require('mini.hues')

math.randomseed(vim.loop.hrtime())
local base_colors = hues.gen_random_base_colors()

if vim.o.background == "dark" then
  hues.setup({
    background = "#14161B",
    foreground = base_colors.foreground,
    n_hues = 8,
    saturation = vim.o.background == 'dark' and 'medium' or 'high',
    accent = 'bg',
  })

  vim.g.colors_name = 'randomhue'
end

if vim.o.background == "light" then
  hues.setup({
    background = "#E0E2EA",
    foreground = base_colors.foreground,
    n_hues = 8,
    saturation = vim.o.background == 'dark' and 'medium' or 'high',
    accent = 'bg',
  })

  vim.g.colors_name = 'randomhue'
end
