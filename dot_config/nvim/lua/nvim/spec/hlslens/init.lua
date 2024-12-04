---@module "hlslens"

---@type LazyPluginSpec
local Spec = {
  "kevinhwang91/nvim-hlslens",

  keys = function(_, keys)
    local function add_key_mappings(maps, cmd_template, options)
      for _, map in ipairs(maps) do
        table.insert(keys, { map, cmd_template:format(map), options or {} })
      end
    end

    local normal_cmd = [[<cmd>execute('normal! ' . v:count1 . '%s')<cr>]]
    local hlslens_cmd = [[<cmd>lua require('hlslens').start()<cr>]]

    add_key_mappings({ "n", "N" }, normal_cmd .. hlslens_cmd, { silent = true })
    add_key_mappings({ "/", "*", "#", "g*", "g#" }, [[%s]] .. hlslens_cmd)

    return keys
  end,

  init = function()
    vim.opt.shortmess:append("S")
  end,

  opts = {
    calm_down = true,
  },
}

return Spec
