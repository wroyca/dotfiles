-- Filetype detection for C++ source files

---@diagnostic disable-next-line: undefined-doc-name
---@type vim.filetype.add
vim.filetype.add ({
  extension = {
    cxx = "cpp", -- source
    hxx = "cpp", -- header
    ixx = "cpp", -- inline
    txx = "cpp", -- template
    mxx = "cpp", -- module
  },
})
