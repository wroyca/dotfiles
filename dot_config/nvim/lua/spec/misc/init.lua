---@module "misc"

-- To map misc/ onto the package manager's expectations, we define a virtual
-- plugin source here and delegate the actual configuration details to the
-- `spec.misc` namespace. The `import` directive instructs the manager to
-- traverse that directory and merge any specifications found there into this
-- one. This allows us to treat the repository as a single artifact for cloning
-- purposes while keeping the configuration logic granular and isolated.
--
-- There is, however, a subtlety with this import mechanism: depending on where
-- this file is located within the Lua path, the `import` directive might
-- attempt to resolve and load this file again, triggering an infinite
-- recursion loop. To handle this, we check if the module is already present in
-- `package.loaded`. If it is, we assume we are inside the recursive step of
-- the import resolution and return an empty specification to terminate the
-- cycle.

---@type LazyPluginSpec
local Spec = {
  "misc", virtual = true, import = "spec.misc",
}

return package.loaded["misc"] and {} or Spec
