---@module "mason-lspconfig"

-- Maintain a set of language servers that should always be installed,
-- regardless of what is currently present in our configuration directory.
-- This is useful for baseline servers (like LuaLS itself) that we expect
-- to be available everywhere in our development environment.
--
local static_servers = {}

-- Figure out the complete set of language servers to ensure are installed.
--
-- We do this by merging our hardcoded static list with the dynamic list
-- discovered from the configuration directory. The idea here is that if a
-- user drops a new server configuration file (e.g., 'rust_analyzer.lua')
-- into the 'lsp' directory, we want mason-lspconfig to automatically pick
-- it up and ensure it is installed without requiring an explicit manual
-- registration step.
--
---@return string[]
local function get_servers ()
  local lsp_dir = vim.fs.joinpath (vim.fn.stdpath ("config") --[[@as string]], "lsp")
  local seen = {}

  -- First, seed the set with our unconditionally required servers.
  --
  for _, s in ipairs (static_servers) do
    seen[s] = true
  end

  -- Next, scan the 'lsp' configuration directory if it exists. We prefer
  -- using libuv (vim.uv or vim.loop for older compat) directly here rather
  -- than going through Vimscript functions like vim.fn.glob(). Not only is
  -- it significantly faster by avoiding the RPC/Vimscript boundary overhead,
  -- but it also gives us more precise control over filesystem traversal and
  -- fails gracefully if the directory does not exist.
  --
  local uv = vim.uv or vim.loop
  local fd = uv.fs_scandir (lsp_dir)

  if fd then
    while true do
      local name, t = uv.fs_scandir_next (fd)
      if not name then
        break
      end

      -- We only care about regular files that end with the '.lua' extension.
      -- Anything else (directories, symlinks to directories, or non-Lua
      -- files) is explicitly ignored.
      --
      if t == "file" and name:match ("%.lua$") then
        -- Note that we must be careful to strip the '.lua' extension to get
        -- the actual server name. Since we already guaranteed the suffix
        -- match above, a simple substring extraction (dropping the last 4
        -- characters) is both safe and highly efficient.
        --
        local s = name:sub (1, -5)
        seen[s] = true
      end
    end
  end

  -- Finally, convert our set back to a standard array structure. The
  -- mason-lspconfig plugin expects a contiguous list of strings for the
  -- 'ensure_installed' option, rather than a dictionary map.
  --
  return vim.tbl_keys (seen)
end

---@type LazyPluginSpec
return {
  "mason-org/mason-lspconfig.nvim",
  event = "VeryLazy",
  opts = {
    ensure_installed = get_servers(),
  },
}
