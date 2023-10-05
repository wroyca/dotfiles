local function git_is_inside_work_tree()
  return os.execute("git rev-parse --is-inside-work-tree > /dev/null 2>&1") ~= nil and true or false
end

---@type LazyPluginSpec
return {
  [[mini.starter]],
  event = [[VimEnter]],
  
  cond = function() 
    if not (vim.fn.argc(-1) == 0) then return false end 
    return true
  end,
  
  config = function()
    local starter = require [[mini.starter]]
    local items = {}

    if git_is_inside_work_tree() then table.insert(items, starter.sections.sessions(5, true)) end
    table.insert(items, starter.sections.recent_files(5, true, false))

    starter.setup({
      items = items,
      content_hooks = {
        starter.gen_hook.adding_bullet(),
        starter.gen_hook.aligning([[center]], [[center]]),
      }
    })

    vim.api.nvim_create_autocmd([[User]], {
      pattern = [[LazyVimStarted]],
      callback = function()
        local stats = require [[lazy]].stats()
        local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
        starter.config.footer = [[Neovim loaded ]] .. stats.count .. [[ plugins in ]] .. ms .. [[ms]]
        pcall(starter.refresh)
      end
    })
  end
}
