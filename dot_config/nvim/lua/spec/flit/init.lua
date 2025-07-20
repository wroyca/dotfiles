---@module "flit"

---@type LazyPluginSpec[]
local Spec = {
  {
    "ggandor/flit.nvim", dependencies = "ggandor/leap.nvim",

    keys = {
      {
        "f",
        mode = { "n", "x", "o" },
        desc = "Flit forward to",
      },
      {
        "F",
        mode = { "n", "x", "o" },
        desc = "Flit backward to",
      },
      {
        "t",
        mode = { "n", "x", "o" },
        desc = "Flit forward till",
      },
      {
        "T",
        mode = { "n", "x", "o" },
        desc = "Flit backward till",
      },
    },

    opts = {
      multiline = false,
      labeled_modes = "nx",
    },

    -- As with leap, flit.nvim direction shifted in a way that discards the
    -- original interface and behavior we had come to rely on.
    --
    -- To be blunt: we are not interested in flit's new vision. We're not
    -- looking for reimagined motion semantics or sweeping conceptual redesigns.
    -- What we *do* care about is a clean, predictable wrapper that extends
    -- basic motions (`f`, `F`, `t`, `T`), which flit.nvim did perfectly, until
    -- it didn't.
    --
    -- To mitigate, we pin to the last known good commit and treat it as frozen
    -- at the version that met our expectations.
    --
    -- Note: this mirrors the situation with leap.nvim (see `leap/init.lua`), in
    -- that should future changes in Neovim itself introduce breakage, long-term
    -- maintenance will fall on us

    commit = "669c5a3c0494b1d032b7366e8935888bfa3953a2",
  },
  {
    "folke/which-key.nvim",
    opts_extend = { "spec" },
  },
}

-- When Flit is first configured, it overrides our key mapping descriptions. We
-- need to explicitly re-register those mappings with which-key to restore the
-- correct descriptions.

Spec[2].opts = {
  spec = Spec[1].keys,
}

return Spec
