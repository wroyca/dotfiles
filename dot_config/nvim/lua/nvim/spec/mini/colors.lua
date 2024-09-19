---@module "mini.colors"

---@type LazyPluginSpec
local Spec = {
  "mini.colors", dev = true, opts = {},

  init = function ()
    pcall(vim.cmd.colorscheme, "randomhue")
  end
}

vim.api.nvim_create_autocmd("ColorScheme", {
  pattern = "*",
  callback = function()
    local function hi(name, opts)
      ---@diagnostic disable-next-line: deprecated
      local is_ok, hl = pcall(vim.api.nvim_get_hl_by_name, name, true)
      if is_ok then
        for k, v in pairs(opts) do
          hl[k] = v
        end
        vim.api.nvim_set_hl(0, name, hl)
      end
    end

    hi("Comment",                          { italic = true })
    hi('@comment.error',                   { italic = true })
    hi('@comment.warning',                 { italic = true })
    hi('@comment.todo',                    { italic = true })
    hi('@comment.note',                    { italic = true })
    hi('Todo',                             { italic = true })
    hi('MiniHipatternsFixme',              { italic = true })
    hi('MiniHipatternsHack',               { italic = true })
    hi('MiniHipatternsNote',               { italic = true })
    hi('MiniHipatternsTodo',               { italic = true })

    hi('gitcommitDiscarded',               { italic = false })
    hi('gitcommitSelected',                { italic = false })
    hi('gitcommitUntracked',               { italic = false })
    hi('DiagnosticUnnecessary',            { italic = false })
    hi('MiniDepsPlaceholder',              { italic = false })
    hi('MiniStarterFooter',                { italic = false })
    hi('MiniStarterInactive',              { italic = false })
    hi('LazyDimmed',                       { italic = false })
    hi('WhichKeyValue',                    { italic = false })
    hi('DashboardFooter',                  { italic = false })
    hi('CmpItemAbbrDeprecated',            { italic = false, strikethrough = true })
    hi('CocDisabled',                      { italic = false })
    hi('CocFadeOut',                       { italic = false })
    hi('DapUIBreakpointsDisabledLine',     { italic = false })
    hi('MasonMuted',                       { italic = false })
    hi('LspCodeLens',                      { italic = false })
    hi('LspCodeLensSeparator',             { italic = false })
    hi('LeapBackdrop',                     { italic = false })

    hi("NormalFloat",                      { bg = '' })
    hi("FloatTitle",                       { bg = '' })
    hi("FloatBorder",                      { bg = '' })
    hi('DiagnosticFloatingError',          { bg = '' })
    hi('DiagnosticFloatingHint',           { bg = '' })
    hi('DiagnosticFloatingInfo',           { bg = '' })
    hi('DiagnosticFloatingOk',             { bg = '' })
    hi('DiagnosticFloatingWarn',           { bg = '' })
    hi("MiniFilesTitleFocused",            { bg = '' })
  end
})

return Spec
