return {
 "nvim-tree/nvim-tree.lua",
  cmd = "NvimTreeToggle",
  keys = {
    {
      -- The NvimTreeToggle command is deliberately not included in whichkey, 
      -- as Ctrl + b is a default keybind in Visual Studio Code to toggle the 
      -- explorer, and I am accustomed to using this keybind.
      --
      "<C-b>", "<cmd>NvimTreeToggle<CR>", desc = "Toggle Explorer View",
    },
  },
  config = function()
    require("nvim-tree").setup({
      renderer = {
        root_folder_modifier = ":t",
        indent_markers = {
          enable = true,
          icons = {
            corner = "└",
            edge = "│",
            none = "",
          },
        },
        icons = {
          git_placement = "signcolumn",
          padding = " ",
          symlink_arrow = "  ",
          show = {
            git = false,
          },
          glyphs = {
            default = "",
            symlink = "",
            folder = {
              default = "",
              empty = "",
              empty_open = "",
              open = "",
              symlink = "",
              symlink_open = "",
              arrow_open = "",
              arrow_closed = "",
            },
            git = {
              unstaged = "",
              staged = "ﰶ",
              unmerged = "",
              renamed = "➜",
              untracked = "●",
              deleted = "﯀",
              ignored = "",
            },
          },
        },
        special_files = {
          "buildfile", -- It can be difficult to locate the "buildfile" when there are numerous files present.
        },
      },
      update_focused_file = {
        enable = true,
        update_cwd = true,
      },
      git = {
        enable = false,
      },
    })
    -- Due to a known bug in NvimTree, NTFS files are incorrectly identified as
    -- executable files, causing them to be highlighted as such even though 
    -- they are not. As a workaround, we disable highlighting for NvimTreeExecFile 
    -- until the bug is fixed.
    --
    vim.cmd[[hi NvimTreeExecFile guifg=#e0e0e0 guibg=none gui=NONE]]
  end
}
