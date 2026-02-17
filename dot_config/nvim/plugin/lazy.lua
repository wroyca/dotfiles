if package.loaded["lazy"] then
  -- https://github.com/folke/lazy.nvim/issues/1180
  return
end

---@type LazyConfig
local opts = {
  defaults = {
    lazy = true,
  },

  pkg = {
    enabled = false,
  },

  rocks = {
    enabled = false,
  },

  readme = {
    enabled = false,
  },

  -- https://github.com/folke/lazy.nvim/issues/1008
  change_detection = {
    notify = false,
  },

  install = {
    colorscheme = { "default" },
  },

  ui = {
    pills = false,
    backdrop = 100,
  },
}

-- Handle the UI window behavior during the bootstrap versus manual usage.
--
-- If this is the first-time install (bootstrap), we want the UI to show
-- progress and then disappear automatically so we are dropped into the editor
-- ready to work. However, if we manually invoked the UI (e.g., running :Lazy
-- update), we want it to stay open so we can inspect the results.
--
-- The heuristic is somewhat tricky because the 'LazyInstall' event fires in
-- both cases. To distinguish them, we check the buffer state and the timing
-- relative to the 'VeryLazy' event.
--
-- We register a hook for both events:
--
-- 1. If 'LazyInstall' fires and we are currently focused on the lazy UI
--    buffer, we assume this is the end of the bootstrap sequence and
--    close the window.
--
-- 2. If 'VeryLazy' fires first (meaning the editor is fully initialized)
--    and we aren't looking at the lazy UI, we assume normal operation.
--    In this case, we remove the closer hook entirely to prevent it from
--    misfiring on future manual updates during this session.
--
vim.api.nvim_create_autocmd ("User", {
  pattern = { "LazyInstall", "VeryLazy" },
  callback = function (ev)
    if ev.match == "LazyInstall" then
      if vim.o.filetype == "lazy" then
        vim.cmd.close ()
      end
    elseif ev.match == "VeryLazy" then
      if vim.o.filetype ~= "lazy" then
        pcall (vim.api.nvim_del_autocmd, ev.id)
      end
    end
  end,
})

require ("lazy").setup ("spec", opts)
