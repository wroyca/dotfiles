---@module "mini"

-- We are dealing with a monolithic repository that bundles multiple logical
-- plugins (modules) into a single codebase. While we could configure everything
-- in this one file, that would result in an unmaintainable monolith of our own.
-- Instead, we want to maintain a "one file per module" configuration structure
-- (e.g., separate files for `mini.comment`, `mini.surround`, etc.).
--
-- To map this preference onto the package manager's expectations, we define
-- the plugin source here but delegate the actual configuration details to the
-- `spec.mini` namespace. The `import` directive instructs the manager to
-- traverse that directory and merge any specifications found there into this
-- one. This allows us to treat the repository as a single artifact for
-- cloning purposes while keeping the configuration logic granular and
-- isolated.
--
-- There is, however, a subtlety with this import mechanism: depending on
-- where this file is located within the Lua path, the `import` directive
-- might attempt to resolve and load this file again, triggering an infinite
-- recursion loop. To handle this, we check if the module is already present
-- in `package.loaded`. If it is, we assume we are inside the recursive step
-- of the import resolution and return an empty specification to terminate the
-- cycle.

---@type LazyPluginSpec
local Spec = {
  "nvim-mini/mini.nvim", import = "spec.mini",
}

return package.loaded["mini"] and {} or Spec
