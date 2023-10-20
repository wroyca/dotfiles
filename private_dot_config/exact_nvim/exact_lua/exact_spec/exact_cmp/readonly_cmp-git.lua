---@type LazyPluginSpec
return {
  [[hrsh7th/cmp-git]],
  event = [[InsertCharPre *.gitcommit]],
  dependencies = {
    [[cmp-conventionalcommits]]
  }
}
