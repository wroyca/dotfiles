vim.filetype.add({
  extension = {
    manifest = "build2-manifest",
    buildfile = "build2-buildfile",
    build2file = "build2-buildfile",
    ["bootstrap.build"] = "build2-buildfile",
    ["root.build"] = "build2-buildfile"
  }
})
