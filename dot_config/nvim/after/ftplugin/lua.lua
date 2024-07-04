vim.bo.comments = [[:---,:--]]

local has_mini_ai, mini_ai = pcall(require, 'mini.ai')
if has_mini_ai then
  vim.b.miniai_config = {
    custom_textobjects = {
      s = { "%[%[().-()%]%]" }
    }
  }
end
