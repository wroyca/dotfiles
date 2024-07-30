---@class hlslens.opts
---@field auto_enable? boolean: Automatically enable `hlslens`. Default is `true`.
---@field enable_incsearch? boolean: Enable incremental search highlighting. Default is `true`.
---@field calm_down? boolean: If `true`, clear highlight when cursor is out of range or when any texts change. Default is `false`.
---@field nearest_only? boolean: If `true`, only highlight the nearest match. Default is `false`.
---@field nearest_float_when? string: When to show the float window for nearest match. Default is 'auto'.
---@field float_shadow_blend? number: The blend level of the float window shadow. Default is `50`.
---@field virt_priority? number: Priority level for virtual text highlighting. Default is `100`.
---@field override_lens? function: Function to override the lens. Default is `nil`.

local Spec = {
  [[hlslens]], keys = { [[*]], [[#]], [[n]], [[N]] },

  ---@type hlslens.opts
  opts = {
    calm_down = true,
  },
}

return Spec
