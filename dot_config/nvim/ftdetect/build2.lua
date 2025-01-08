---@diagnostic disable-next-line: undefined-doc-name
---@type vim.filetype.add
vim.filetype.add ({
  extension = {
    build = "buildfile",
  },
  pattern = {
    ["manifest"] = "manifest",
    ["buildfile"] = "buildfile",
    ["build2file"] = "buildfile",
  },
})
