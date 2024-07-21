---@alias __extra_ai_spec_return function Function implementing |MiniAi-textobject-specification|.
---@alias __extra_pickers_local_opts table|nil Options defining behavior of this particular picker.

---@type LazyPluginSpec
return {
  [[mini.ai]],

  keys = {
    { [[a]], mode = { [[x]], [[o]] } },
    { [[i]], mode = { [[x]], [[o]] } }
  },

  opts = function()
    local gen_ai_spec = require [[mini.extra]].gen_ai_spec

    --- Current buffer diagnostic textobject for CoC
    ---
    --- Notes:
    --- - Both `a` and `i` textobjects return |vim.fn["CocAction"]("diagnosticList")| output for the
    ---   current buffer. It is modified to fit |MiniAi-textobject-specification|.
    ---
    ---@return __extra_ai_spec_return
    gen_ai_spec.diagnostic = function()
      return function()
        local cur_diag = vim.fn["CocAction"]("diagnosticList")
        local regions = {}
        for _, diag in ipairs(cur_diag) do
          local from = { line = diag.lnum, col = diag.col }
          local to = { line = diag.end_lnum, col = diag.end_col - 1 }
          if to.line == nil or to.col == nil then
            to = { line = diag.lnum, col = diag.col - 1 }
          end
          table.insert(regions, { from = from, to = to })
        end
        return regions
      end
    end

    return {
      custom_textobjects = {
        B = gen_ai_spec.buffer(),
        D = gen_ai_spec.diagnostic(),
        I = gen_ai_spec.indent(),
        L = gen_ai_spec.line(),
        N = gen_ai_spec.number()
      }
    }
  end
}
