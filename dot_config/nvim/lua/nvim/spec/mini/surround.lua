---@module "mini.surround"

---@type LazyPluginSpec
local Spec = {
  "mini.surround", dev = true, lazy = false,

  keys = function(_, keys)
    local spec = require("lazy.core.config").spec.plugins["mini.surround"]
    local opts = require("lazy.core.plugin").values(spec, "opts", false)

    local mappings = {
      { opts.mappings.add },
      { opts.mappings.delete },
      { opts.mappings.replace },
    }

    mappings = vim.tbl_filter(function(m) return m[1] and #m[1] > 0 end, mappings)
    return vim.list_extend(mappings, keys)
  end,

  opts = {
    mappings = {
      add = "gsa",
      delete = "gsd",
      replace = "gsr",
      find = "",
      find_left = "",
      highlight = "",
      update_n_lines = "",
    },
    search_method = "next",
    silent = true,
  },

  config = function(_, opts)
    -- HACK:
    --
    -- Mini.Surround's `add` functionality operates with the 'a' text object by
    -- default, which includes surrounding whitespace when capturing. While
    -- this behavior may be acceptable in some contexts, for a surround plugin,
    -- we typically want text objects to focus solely on the text between
    -- delimiters.
    --
    -- Since Mini.Surround does not directly expose the necessary settings, we
    -- need to use introspection to access its internal H table, which is
    -- typically private, and then modify the `make_operator` function to use
    -- 'i' (inner) by default instead of 'a' (around).
    --
    local H = {}
    local function hook(_)
      local information = debug.getinfo(2, "nS")
      if information and information.name == "make_operator" then
        local make_operator = debug.getinfo(2, "f").func
        local i = 1
        while true do
          local name, value = debug.getupvalue(make_operator, i)
          if name == "H" then
            H = value
            H.make_operator = function(task, direction, search_method, ask_for_textobject, textobject)
              return function()
                textobject = textobject or ''
                if H.is_disabled() then return [[\<Esc>]] end
                H.cache = { count = vim.v.count1, direction = direction, search_method = search_method }
                vim.o.operatorfunc = 'v:lua.MiniSurround.' .. task
                return '<Cmd>echon ""<CR>g@' .. (ask_for_textobject and textobject or ' ')
              end
            end
            H.apply_hook_logic = function()
              local config = H.get_config()
              local m = config.mappings
              local expr_map = function(lhs, rhs, desc)
                value.map("n", lhs, rhs, { expr = true, desc = desc })
              end
              expr_map(m.add, value.make_operator("add", nil, nil, true, 'i'), "Add surrounding")
              debug.sethook()
            end
            break
          end
          i = i + 1
        end
      end
    end

    debug.sethook(hook, "c")
    require("mini.surround").setup(opts)
    H.apply_hook_logic()
  end,

  specs = {
    "folke/which-key.nvim", opts = { spec = { { { "gs", group = "Surround" } } } },
  },
}

return Spec
