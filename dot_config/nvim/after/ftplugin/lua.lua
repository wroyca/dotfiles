vim.bo.comments = [[:---,:--]]
if vim.fn.executable [[stylua]] == 1 then
  vim.opt_local.formatprg = [[stylua -]]
end

vim.opt_local.include = [[\v<((do|load)file|require|reload)[^''"]*[''"]\zs[^''"]+]]
vim.opt_local.includeexpr = [[substitute(v:fname,'\\.','/','g')]]
vim.opt_local.suffixesadd:prepend [[.lua]]
vim.opt_local.suffixesadd:prepend [[init.lua]]

for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
  vim.opt_local.path:append(vim.fs.joinpath(path --[[ @as string ]], [[lua]]))
end

local has_mini_ai, _ = pcall(require, [[mini.ai]])
if has_mini_ai then
  vim.b.miniai_config = {
    custom_textobjects = {
      s = { "%[%[().-()%]%]" }
    }
  }
end
