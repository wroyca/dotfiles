return {
	"nvim-telescope/telescope.nvim",
	lazy = false, -- make sure we load this during startup
	cmd = { "Telescope" },
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ 
		  "nvim-telescope/telescope-fzf-native.nvim", build = "make" 
		},
	},
	config = function()
		local telescope = require('telescope.builtin')
    vim.keymap.set('n', '<leader>fd', telescope.diagnostics, {})
    vim.keymap.set('n', '<leader>fr', telescope.resume, {})
    vim.keymap.set('n', '<leader>ff', telescope.find_files, {})
    vim.keymap.set('n', '<leader>fg', telescope.live_grep, {})
    vim.keymap.set('n', '<leader>fc', telescope.buffers, {})
    vim.keymap.set('n', '<leader>fh', telescope.help_tags, {})
		require("telescope").setup({
			picker = {
				hidden = false,
			},
			defaults = {
				vimgrep_arguments = {
					"rg",
					"--color=never",
					"--no-heading",
					"--with-filename",
					"--line-number",
					"--column",
					"--no-ignore",
					"--smart-case",
					"--hidden",
				},
				defaults = require("telescope.themes").get_ivy({
          set_env = { ["COLORTERM"] = "truecolor" },
          max_results = 100,
          selection_strategy = "reset",
          sorting_strategy = "ascending",
          color_devicons = true,
          scroll_strategy = "limit",
          layout_strategy = "vertical",
          initial_mode = "insert",
          layout_config = {
            width = 0.99,
            height = 0.3,
            preview_cutoff = 20,
            prompt_position = "bottom",

            horizontal = {
              preview_width = 0.65,
            },
            vertical = {
              preview_width = 0.99,
              width = 0.99,
              height = 100,
              preview_height = 0.6,
            },
            center = {
              height = 0.30,
              preview_cutoff = 40,
              prompt_position = "top",
              width = 0.99,
            },
            flex = {
              preview_width = 0.65,
              horizontal = {},
            },
            bottom_pane = {
              height = 0.3,
              preview_cutoff = 130,
              prompt_position = "top",
            },
          },
          borderchars = {
            prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
            results = { " " },
            preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
          },
        }),
				file_sorter = require("telescope.sorters").get_fuzzy_file,
				file_ignore_patterns = { ".git/" },
				generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
				path_display = { "absolute" },
				file_previewer = require("telescope.previewers").vim_buffer_cat.new,
				grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
				qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
				buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
				mappings = {
					i = {
						["<Tab>"] = "move_selection_next",
						["<S-Tab>"] = "move_selection_previous",
					},
					n = {
						["<Tab>"] = "move_selection_next",
						["<S-Tab>"] = "move_selection_previous",
					},
				},
			},
		})
		require("telescope").load_extension("fzf")
	end,
}
