---@module "leap"

---@type LazyPluginSpec
local Spec = {
  "ggandor/leap.nvim",

  keys = {
    {
      "s",
      "<Plug>(leap-forward)",
      mode = { "n", "x", "o" },
      desc = "Leap forward",
    },
    {
      "S",
      "<Plug>(leap-backward)",
      mode = { "n", "x", "o" },
      desc = "Leap backward",
    },
  },

  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },

  -- Leap redesign fundamentally altered the semantics of the `s` motion,
  -- rendering it omnidirectional and thereby severing its original
  -- forward/backward distinction. This change compromises the very principles
  -- that once made Leap technically robust: its motions were deterministic,
  -- directionally constrained, and predictably composable with operators.
  --
  -- Consider, for instance, the clarity of `dsx` (delete to a forward match)
  -- versus `DSx` (delete to a backward one). These commands communicated both
  -- intent and direction unambiguously. That clarity is now gone. Stripped of
  -- directionality, motions become semantically vague, and their interaction
  -- with operators grows increasingly difficult to reason about.
  --
  -- The only responsible course of action is to pin Leap to a known-good
  -- commit one which still honors these original guarantees. Should future
  -- changes in Neovim itself introduce breakage, maintenance will necessarily
  -- fall to those of us unwilling to forfeit precision for upstream
  -- conformity.
  --
  -- This is far from ideal, but in matters of correctness, fidelity must take
  -- precedence over convenience.

  commit = "5ae080b646021bbb6e1d8715b155b1e633e28166",
}

return Spec
