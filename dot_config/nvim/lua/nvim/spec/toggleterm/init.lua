---@module "toggleterm"

local Spec = {
  "akinsho/toggleterm.nvim",
  keys = function()
    local terminal = require("toggleterm.terminal").Terminal
    local magit = terminal:new({
      cmd = "emacs -nw --init-directory=~/.config/nvim/elisp --magit .",
      direction = "tab",
    })
    return {
      { "<leader>g", function() magit:toggle() end, desc = "Magit" }
    }
  end,
  opts = {
    autochdir = true,
    shade_terminals = false,
  },
}

function _G.set_terminal_keymaps()
  local opts = { buffer = 0 }

  -- Restore <Esc> to switch to Normal mode
  vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)

  -- Restore <C-o> and <C-i> for jumplist navigation
  vim.keymap.set("t", "<C-o>", [[<C-\><C-n><C-o>]], opts)
  vim.keymap.set("t", "<C-i>", [[<C-\><C-n><C-i>]], opts)

  -- Allow colon to trigger command mode
  vim.keymap.set("t", ":", [[<C-\><C-n>:]], opts)
end

vim.api.nvim_create_autocmd({ "TermOpen" }, {
  pattern = "term://*toggleterm#*",
  callback = function() set_terminal_keymaps() end,
})

vim.api.nvim_create_autocmd({ "TermEnter" }, {
  pattern = "term://*toggleterm#*",
  callback = function() vim.cmd.startinsert() end,
})

vim.api.nvim_create_autocmd({ "BufEnter" }, {
  pattern = "term://*toggleterm#*",
  callback = function() vim.cmd.startinsert() end,
})

--
--

-- Prototype utility to convert Neovim colorscheme to Emacs colorscheme
-- asynchronously.

local uv = vim.loop

--- Get accent color from the MiniHues palette.
--- @param accent string: The accent key for the color palette.
--- @return string: The corresponding color value or "nil" if not found.
local function get_accent_color(accent)
  local mini_hues = require "mini.hues"
  local palette = mini_hues.make_palette {}
  return palette[accent] or "nil"
end

--- Replace placeholder colors in a file with actual accent colors asynchronously.
--- @param file_path string: The path of the file to be modified.
local function replace_with_accent(file_path)
  uv.fs_open(file_path, "r", 438, function(err, fd)
    if err then
      print("Error: Unable to open file " .. file_path)
      return
    end
    uv.fs_fstat(fd, function(_, stat)
      assert(stat ~= nil)
      uv.fs_read(fd, stat.size, 0, function(_, data)
        uv.fs_close(fd, function()
          assert (data ~= nil)
          -- FIXME: We're using this approach because we couldn't get gsub to
          -- work with word boundaries.
          data = data:gsub("p%.bg_mid2",   get_accent_color "bg_mid2")
          data = data:gsub("p%.bg",        get_accent_color "bg")
          data = data:gsub("p%.fg_mid2",   get_accent_color "fg_mid2")
          data = data:gsub("p%.fg",        get_accent_color "fg")
          data = data:gsub("p%.red_bg",    get_accent_color "red_bg")
          data = data:gsub("p%.orange_bg", get_accent_color "orange_bg")
          data = data:gsub("p%.yellow_bg", get_accent_color "yellow_bg")
          data = data:gsub("p%.green_bg",  get_accent_color "green_bg")
          data = data:gsub("p%.cyan_bg",   get_accent_color "cyan_bg")
          data = data:gsub("p%.azure_bg",  get_accent_color "azure_bg")
          data = data:gsub("p%.blue_bg",   get_accent_color "blue_bg")
          data = data:gsub("p%.purple_bg", get_accent_color "purple_bg")
          data = data:gsub("p%.red",       get_accent_color "red")
          data = data:gsub("p%.orange",    get_accent_color "orange")
          data = data:gsub("p%.yellow",    get_accent_color "yellow")
          data = data:gsub("p%.green",     get_accent_color "green")
          data = data:gsub("p%.cyan",      get_accent_color "cyan")
          data = data:gsub("p%.azure",     get_accent_color "azure")
          data = data:gsub("p%.blue",      get_accent_color "blue")
          data = data:gsub("p%.purple",    get_accent_color "purple")
          uv.fs_open(file_path, "w", 438, function(_, fd_w)
            uv.fs_write(fd_w, data, 0, function() uv.fs_close(fd_w) end)
          end)
        end)
      end)
    end)
  end)
end

--- Copy a file asynchronously from source to destination.
--- @param src string: The path of the source file.
--- @param dest string: The path of the destination file.
--- @param callback function: Callback function to execute after completion.
local function copy_file(src, dest, callback)
  uv.fs_open(src, "r", 438, function(err, fd)
    if err then
      print("Error: Unable to open source file " .. src)
      callback(false)
      return
    end
    uv.fs_fstat(fd, function(_, stat)
      assert(stat ~= nil)
      uv.fs_read(fd, stat.size, 0, function(_, data)
        uv.fs_close(fd, function()
          uv.fs_open(dest, "w", 438, function(_, fd_w)
            uv.fs_write(fd_w, data, 0, function()
              uv.fs_close(fd_w, function()
                callback(true)
              end)
            end)
          end)
        end)
      end)
    end)
  end)
end

local function on_colorscheme_change()
  local dest_file = "/home/wroy/.config/nvim/elisp/early-init.el"
  local src_file = "/home/wroy/.local/share/chezmoi/dot_config/nvim/elisp/early-init.el"
  uv.fs_unlink(dest_file, function()
    copy_file(src_file, dest_file, function(success)
      if success then
        replace_with_accent(dest_file)
      end
    end)
  end)
end

vim.api.nvim_create_autocmd({"ColorScheme"}, {
  callback = on_colorscheme_change
})

return Spec
