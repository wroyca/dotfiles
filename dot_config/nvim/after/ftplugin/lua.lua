if vim.bo.filetype ~= "lua" then
  return
end

vim.b.miniai_config = {
  custom_textobjects = {
    s = {
      "%[%[().-()%]%]",
    },
  },
}

vim.b.minisurround_config = {
  custom_surroundings = {
    s = {
      input = {
        "%[%[().-()%]%]",
      },
      output = {
        left = "[[",
        right = "]]",
      },
    },
  },
}

if _G.MiniSplitjoin ~= nil then
  local gen_hook = MiniSplitjoin.gen_hook
  local curly = {
    brackets = {
      "%b{}",
    },
  }

  -- Add trailing comma when splitting inside curly brackets
  local add_comma_curly = gen_hook.add_trailing_separator (curly)

  -- Delete trailing comma when joining inside curly brackets
  local del_comma_curly = gen_hook.del_trailing_separator (curly)

  -- Pad curly brackets with single space after join
  local pad_curly = gen_hook.pad_brackets (curly)

  vim.b.minisplitjoin_config = {
    split = {
      hooks_post = {
        add_comma_curly,
      },
    },
    join = {
      hooks_post = {
        del_comma_curly,
        pad_curly,
      },
    },
  }
end
