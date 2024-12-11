local hues = require ("mini.hues")
local seed = math.randomseed (vim.loop.hrtime ())
local base = hues.gen_random_base_colors ()

-- In Lua, the if statement and the logical operators and/or behave differently
-- when controlling flow and variable assignment. Here, if must be used instead
-- of and/or to ensure proper behavior when switching variants with lumen.

if vim.o.background == "dark" then
  hues.setup ({
    background = "#14161B",
    foreground = base.foreground,
    saturation = "medium",

    n_hues = 8,
    accent = "bg",
  })
end

if vim.o.background == "light" then
  hues.setup ({
    background = "#E0E2EA",
    foreground = base.foreground,
    saturation = "high",

    n_hues = 8,
    accent = "bg",
  })
end

vim.g.colors_name = "randomhue"
