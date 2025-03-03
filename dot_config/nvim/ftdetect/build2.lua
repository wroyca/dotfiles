-- Filetype detection for build2 build system files
-- https://build2.org/build2/doc/build2-build-system-manual.xhtml

---@diagnostic disable-next-line: undefined-doc-name
---@type vim.filetype.add
vim.filetype.add ({
  extension = {
    build = "buildfile",
  },

  pattern = {
    ["manifest"] = "manifest", -- package manifest file
    ["buildfile"] = "buildfile", -- build configuration
    ["build2file"] = "buildfile", -- alternative build configuration
  },
})
