---@type LazyPluginSpec
return {
  [[telescope.nvim]],
  event = [[VeryLazy]],
  cmd = [[Telescope]],
  dependencies = {
    { [[nvim-telescope/telescope-fzf-native.nvim]], enabled = vim.fn.executable [[make]] == 1, build = [[make]] },
    { [[nvim-telescope/telescope-frecency.nvim]] },
  { [[nvim-telescope/telescope-project.nvim]] },
  },
  keys = {
    { [[<leader><leader>]], [[<cmd>Telescope builtin include_extensions=true<cr>]],  desc = [[Telescope]] },
  },

  opts = function()
    -- Make telescope layout consistent with CoC, QuickFix and LocList, among
    -- others. Note that we wrap everything within paranoia checks, as
    -- Telescope extensions might attempt to query invalid states, which could
    -- lead to unexpected behavior.
    --
    require [[telescope.pickers.layout_strategies]].bottom = function(picker, max_columns, max_lines, layout_config)
      local layout = require [[telescope.pickers.layout_strategies]].bottom_pane(picker, max_columns, max_lines, layout_config)

      -- Exclude titles from our layout, transforming Telescope into a "list" of items rather than a full-fledged widget.
      --
      if layout.prompt  then layout.prompt.title  = [[]] end
      if layout.results then layout.results.title = [[]] end
      if layout.preview then layout.preview.title = [[]] end

      -- Account for missing titles (as mentioned above), which would lead to
      -- additional padding being wasted.
      --
      if layout.results then layout.results.height = layout.results.height + 1 end
      if layout.preview then layout.preview.height = layout.preview.height + 2 end

      -- Force our prompt to be in Neovim command-line area, at the very
      -- bottom. Note that we must set a very high number for the line so that
      -- it stays at the bottom regardless of Neovim window size (it is
      -- relative).
      --
      if layout.prompt then layout.prompt.col = 1 end
      if layout.prompt then layout.prompt.line = 1000 end
      return layout
    end

    require [[telescope]].load_extension [[fzf]]
    require [[telescope]].load_extension [[frecency]]
    require [[telescope]].load_extension [[project]]

    return {
      defaults = {
        layout_strategy = [[bottom]],
        layout_config = {
          -- Increase by 2 to compensate for the titles removal. Now, with a
          -- total of 10 (8+2), 8 elements will be displayed (whereas 8 would
          -- result in only 6 elements being displayed as a side effect from
          -- removing titles).
          --
          height = 8 + 2,

          -- Although we position the prompt at the bottom in our custom layout,
          -- it remains linked to the anchor behind it. That is, we must ensure
          -- that the anchor is set to bottom; otherwise, the layout will be
          -- wrong.
          ---
          prompt_position = [[bottom]]
        },

        -- Remove n/N count from our prompt. This should have been an option;
        -- figuring this out was more difficult than it needed to be.
        --
        get_status_text = function(self, opts) return [[]] end,

        -- Mimick neovim prompt prefix since we're our layout is displayed in the
        -- cmdline area.
        --
        prompt_prefix = [[:]],

        -- Omit displaying any icons or symbols as a selection caret. Note that
        -- whitespace is necessary here to accommodate the two-byte width icon
        -- that is typically used.
        --
        selection_caret = [[  ]],

        -- Display borders, but set their characters to whitespace to make them
        -- invisible, serving only to add additional padding in the layout.
        --
        border = true,
        borderchars = {
          [[ ]],
          [[ ]],
          [[ ]],
          [[ ]],
          [[ ]],
          [[ ]],
          [[ ]],
          [[ ]]
        },

        multi_icon = [[⋅]],
        path_display = { [[smart]] },
        sorting_strategy = [[ascending]]
      },

      -- Extension(s)
      --
      extensions = {
        project = {
          base_dirs = {
            '~/Projects'
          }
        }
      }
    }
  end
}
