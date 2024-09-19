local nvim_create_autocmd = vim.api.nvim_create_autocmd

nvim_create_autocmd("FileType", {
  desc = "Ensure editorconfig settings take highest precedence",
  callback = function(ev)
    if vim.F.if_nil(vim.b.editorconfig, vim.g.editorconfig, true) then
      local editorconfig_avail, editorconfig = pcall(require, "editorconfig")
      if editorconfig_avail then
        editorconfig.config(ev.buf)
      end
    end
  end,
})

nvim_create_autocmd("BufWritePre", {
  desc = "Automatically create parent directories if they don't exist when saving a file",
  callback = function(ev)
    local buf_is_valid_and_listed = vim.api.nvim_buf_is_valid(ev.buf) and vim.bo[ev.buf].buflisted
    if buf_is_valid_and_listed then
      vim.fn.mkdir(vim.fn.fnamemodify(vim.uv.fs_realpath(ev.match) or ev.match, ":p:h"), "p")
    end
  end,
})

nvim_create_autocmd({ "InsertLeave", "WinEnter" }, {
  desc = "Show cursor line in active window",
  callback = function(event)
    if vim.bo[event.buf].buftype == "" then
      vim.opt_local.cursorline = true
    end
  end,
})

nvim_create_autocmd({ "InsertEnter", "WinLeave" }, {
  desc = "Hide cursor line in inactive window",
  callback = function()
    vim.opt_local.cursorline = false
  end,
})

nvim_create_autocmd({ "VimResized" }, {
  desc = "Resize splits if window got resized",
  callback = function()
    local current_tab = vim.api.nvim_get_current_tabpage()
    vim.cmd.tabdo("wincmd =")
    vim.api.nvim_set_current_tabpage(current_tab)
  end,
})

-- Apply and restore local patches on plugin update/install
-- Credit: @Bekaboo in Bekaboo/nvim
nvim_create_autocmd("User", {
  pattern = { "LazyInstall*", "LazyUpdate*" },
  group = vim.api.nvim_create_augroup("LazyPatches", { clear = true }),
  desc = "Reverse/apply local patches on updating/installing plugins",
  callback = function(info)
    local patches_path = vim.fn.stdpath("config") .. "/patches"
    print(patches_path)
    for patch in vim.fs.dir(patches_path) do
      local patch_path = patches_path .. "/" .. patch
      local plugin_path = vim.fn.stdpath("data") .. "/lazy/" .. (patch:gsub("%.patch$", ""))

      local gitcmd = function(path, cmd)
        local shell_args = { "git", "-C", path, unpack(cmd) }
        local shell_out = vim.fn.system(shell_args)
        return {
          success = (vim.v.shell_error == 0),
          output = shell_out,
        }
      end

      if vim.fn.filereadable(patch_path) then
        if info.match:match("Pre$") and gitcmd(plugin_path, { "diff", "--stat" }).output ~= "" then
          vim.notify("reverting plugin patch" .. patch_path)
          gitcmd(plugin_path, { "apply", "--reverse", "--ignore-space-change", patch_path })
        else
          vim.notify("applying plugin patch" .. patch_path)
          gitcmd(plugin_path, { "apply", "--ignore-space-change", patch_path })
        end
      end
    end
  end,
})

return {}
