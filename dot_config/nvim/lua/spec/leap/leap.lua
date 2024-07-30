--- @class leap.opts
--- @field preview_filter? string|nil Filter for previewing, or `nil`.
--- @field max_phase_one_targets? number|nil Maximum number of phase one targets, or `nil`.
--- @field max_highlighted_traversal_targets? number Maximum number of highlighted traversal targets.
--- @field equivalence_classes? table List of equivalence classes, typically for characters.
--- @field substitute_chars? table Mapping for characters that should be substituted.
--- @field safe_labels? table List of safe labels used in operations.
--- @field labels? table List of all possible labels used in operations.
--- @field special_keys? table Mapping of special keys used for navigation and grouping.
--- @field special_keys.next_target? string Key for moving to the next target.
--- @field special_keys.prev_target? table List of keys for moving to the previous target.
--- @field special_keys.next_group? string Key for moving to the next group.
--- @field special_keys.prev_group? table List of keys for moving to the previous group.
--- @field highlight_unlabeled_phase_one_targets? boolean Whether to highlight unlabeled phase one targets.
--- @field case_sensitive? boolean Whether the operation is case sensitive.
--- @field current_call? table Current call data, typically used for tracking state.

---@LazyPluginSpec
local Spec = {
  [[leap]], keys = { [[s]], [[S]], [[gs]] },

  ---@type leap.opts
  opts = {
    highlight_unlabeled_phase_one_targets = true,
  }
}

return Spec
