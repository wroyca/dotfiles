---@diagnostic disable: undefined-doc-name
---@type vim.filetype.add
vim.filetype.add ({
  extension = {
    build = "buildfile",
  },
  pattern = {
    ["manifest"] = "manifest",
    ["packages%.manifest"] = "packages-manifest",
    ["repositories%.manifest"] = "repositories-manifest",
    ["buildfile"] = "buildfile",
  },
})
