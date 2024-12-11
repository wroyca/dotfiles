vim.api.nvim_create_autocmd ({ "BufRead", "BufNewFile" }, {
  pattern = {
    "manifest",
    "buildfile",
    "build2file",
    "bootstrap.build",
    "root.build",
  },
  callback = function ()
    if vim.fn.expand ("<afile>") == "manifest" then
      vim.bo.filetype = "build2-manifest"
    else
      vim.bo.filetype = "build2-buildfile"
    end
  end,
})
