---@type LazyPluginSpec
return {
  [[leap]],

  keys = { [[s]], [[S]], [[gs]] },
  opts = {
    highlight_unlabeled_phase_one_targets = true,
  },

  config = function(_, opts)
    require [[leap]].setup(opts)
    require [[leap]].add_default_mappings(true)
  end
}
