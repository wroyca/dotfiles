local has_mini_ai, _ = pcall(require, "mini.ai")
if has_mini_ai then
  vim.b.miniai_config = {
    custom_textobjects = {
      s = { "%[%[().-()%]%]" }
    }
  }
end
