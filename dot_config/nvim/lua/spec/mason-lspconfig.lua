---@module "mason-lspconfig"

-- Servers we always want, regardless of filesystem state.
local static_servers = {}

---Scans the config/lsp directory and merges it with static servers.
---@return string[]
local function get_servers()
  local lsp_dir = vim.fs.joinpath(vim.fn.stdpath("config"), "lsp")
  local seen = {}
  for _, server in ipairs(static_servers) do
    seen[server] = true
  end
  if vim.fn.isdirectory(lsp_dir) == 1 then
    local files = vim.fn.glob(vim.fs.joinpath(lsp_dir, "*.lua"), false, true)
    for _, file in ipairs(files) do
      local server_name = vim.fn.fnamemodify(file, ":t:r")
      seen[server_name] = true
    end
  end
  return vim.tbl_keys(seen)
end

---@type LazyPluginSpec
return {
  "mason-org/mason-lspconfig.nvim",
  event = "VeryLazy",
  opts = {
    ensure_installed = get_servers(),
  },
}
