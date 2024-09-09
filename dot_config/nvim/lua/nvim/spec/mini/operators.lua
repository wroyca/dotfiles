---@module "mini.operators"

---@type LazyPluginSpec
local Spec = {
  "mini.operators", dev = true, opts = {},

  keys = {
    { "g=",  mode = { "n", "x" }, desc = "Evalute motion" },
    { "g==", mode = { "n", "x" }, desc = "Evalute line" },
    { "gm",  mode = { "n", "x" }, desc = "Multiply motion" },
    { "gmm", mode = { "n", "x" }, desc = "Multiply line" },
    { "go",  mode = { "n", "x" }, desc = "Sort motion" },
    { "goo", mode = { "n", "x" }, desc = "Sort line" },
    { "gp",  mode = { "n", "x" }, desc = "Replace motion" },
    { "gpp", mode = { "n", "x" }, desc = "Replace line" },
    { "gs",  mode = { "n", "x" }, desc = "Exchange motion" },
    { "gss", mode = { "n", "x" }, desc = "Exchange line" },
  },
}

return Spec
