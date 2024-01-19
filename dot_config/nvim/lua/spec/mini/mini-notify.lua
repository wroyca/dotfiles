---@type LazyPluginSpec
return {
  [[mini.notify]],
  dev = true,
  config = true,

  -- We're not interested in the whole notification machinery, but it handles
  -- active LSP clients' $/progress endpoint, which we do actually care about,
  -- so enable it only from LspAttach and onward.
  --
  event = [[LspAttach]]
}
