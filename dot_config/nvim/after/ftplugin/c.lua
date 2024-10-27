if vim.bo.filetype ~= "c" then return end
if vim.fn.executable "clang-format" == 1 then
  vim.opt_local.formatprg = "clang-format --assume-filename=.c"
end

-- Extract clang system include directories which will be searched when using
-- the gf, [f, ]f, ^Wf, :find, :sfind, :tabfind and other commands.
--
vim.opt_local.path = (function()
  local t = "/tmp/hello.c"
  local c = '#include <stdio.h>\nint main() { return 0; }'
  local k = "clang" .. "_" .. t

  ftplugin_c_cache = ftplugin_c_cache or {}
  if ftplugin_c_cache[k] then
    return ftplugin_c_cache[k]
  end

  assert(io.open(t, "w")):write(c):close()

  local handle = assert(io.popen('clang -### ' .. t .. ' 2>&1'))
  local include_paths = {}

  for line in handle:lines() do
    for path in string.gmatch(line, '"-internal%-isystem" "(.-)"') do
      table.insert(include_paths, path)
    end
  end

  os.remove(t)
  handle:close()
  ftplugin_c_cache[k] = include_paths

  return table.concat(include_paths, ",")
end)()
