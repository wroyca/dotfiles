---@type LazyPluginSpec
return {
  {
    [[nvim-telescope/telescope.nvim]],
    name = [[telescope]],
    cmd = [[Telescope]],
    opts = {
      defaults = {
        border = false,
        path_display = { [[smart]] },
        sorting_strategy = [[ascending]],
        entry_prefix = [[  ]],
        prompt_prefix = [[  ]],
        selection_caret = [[  ]],
        layout_strategy = [[bottom_pane]],
        layout_config = {
          height = 8,
          prompt_position = [[bottom]]
        }
      },
      pickers = {
        colorscheme = {
          enable_preview = true
        }
      }
    }
  },
  {
    [[nvim-telescope/telescope-fzf-native.nvim]],
    name = [[telescope-fzf-native]],
    build = [[make]]
  }
}
