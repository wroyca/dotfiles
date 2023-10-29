---@type LazyPluginSpec
return {
  ---@diagnostic disable-next-line: assign-type-mismatch
  {
    [[echasnovski/mini.pick]],
    enabled = false
  },
  {
    [[nvim-telescope/telescope.nvim]],
    enabled = true,
    cmd = [[Telescope]],
    dependencies = [[nvim-lua/plenary.nvim]],
    opts = {
      defaults = {
        layout_strategy = [[bottom_pane]],
        layout_config = {
          height = 15,
          preview_cutoff = 120,
          prompt_position = [[bottom]]
        },
        border = false,
        path_display = { [[smart]] },
        sorting_strategy = [[ascending]],
      }
    }
  }
}
