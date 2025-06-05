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

  -- Recent changes have redefined the s motion in Leap as omnidirectional,
  -- discarding the forward/backward distinction that previously anchored its
  -- motion model. This breaks the core design principles that made Leap
  -- technically sound: motions were deterministic, directionally constrained,
  -- and precisely composable with operators. For instance, dsx (delete to a
  -- forward target) and DSx (delete to a backward target) expressed clearly
  -- defined, direction-aware operations. That clarity is now lost. Without
  -- directionality, motion-operator combinations become ambiguous and
  -- unpredictable.
  --
  -- The only viable response is to lock Leap to a known-good commit. If later
  -- changes in Neovim cause breakage, maintenance will fall to me who care
  -- about preserving its original guarantees. This is not ideal, but
  -- correctness must take priority over passive alignment with upstream.

  commit = "5ae080b646021bbb6e1d8715b155b1e633e28166",
}

return Spec
