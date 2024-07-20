vim.filetype.add({
  extension = {
    cxx = [[cpp]],
    hxx = [[cpp]],
    ixx = [[cpp]],
    txx = [[cpp]],
    mxx = [[cpp]]
  },

  ["in"] = function(path)
    local _, _, filename = path:find(".*/(.*)$")
    local _, _, captured = filename:find("(.*).in$")

    return vim.filetype.match({ filename = captured })
  end
})
