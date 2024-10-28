local hues = require("mini.hues")
local seed = math.randomseed(vim.loop.hrtime())
local base = hues.gen_random_base_colors()

-- NOTE: Using 'if' here instead of 'and/or' is necessary; switching variants
-- with lumen will not work otherwise.

if vim.o.background == "dark" then
  hues.setup({
    background = "#14161B",
    foreground = base.foreground,
    saturation = "medium",

    n_hues = 8, accent = "bg",
  })
end

if vim.o.background == "light" then
  hues.setup({
    background = "#E0E2EA",
    foreground = base.foreground,
    saturation = "high",

    n_hues = 8, accent = "bg",
  })
end

vim.g.colors_name = "randomhue"
