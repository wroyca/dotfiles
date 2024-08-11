---@module "mini.notify"

---@class CocNotify
---@field id number|nil The existing notification ID (if any).
---@field indexing boolean The indexing process current state.
---@field autocmd number|nil The existing autocommand ID (if any).
local CocNotify = {} CocNotify.__index = CocNotify

---@return CocNotify instance A new instance of CocNotify.
function CocNotify:new()
  return setmetatable({ id = nil, indexing = false, autocmd = nil }, CocNotify)
end

---Setup the autocommand to track CocStatusChange events.
function CocNotify:setup()
  self.autocmd = vim.api.nvim_create_autocmd("User", {
    pattern = { "CocStatusChange" },
    callback = function()
      self:callback()
    end
  })
end

---Parse CoC status to sanitize any unwanted inputs (e.g. ignored scopeUri).
---@param status string The status string from CoC.
---@return string status The sanitized status string.
function CocNotify:parse(status)
  local uniquify = function(input)
    local duplicates = {}
    local uniquifying = input:gsub("%S+", function(word)
      if duplicates[word] then
        return ""
      else
        duplicates[word] = true
        return word
      end
    end)

    return uniquifying:match("^(.-%s%s)")
  end

  -- TODO: Figure out how to retrieve the linter dynamically?
  local known_linters = { "stylua", "eslint", "prettier", "flake8", "rubocop" }
  local strip_linter_names = function(input)
    for _, linter in ipairs(known_linters) do
      input = input:gsub(linter, "")
    end
    return input
  end

  return strip_linter_names(uniquify(status))
end

---Updates or creates CocNotify status notification.
---@param status string CoC status.
function CocNotify:update(status)
  local notification = self:parse(status)

  -- TODO: Figure out a way to interpolate the current progress
  -- towards the target progress
  if self.id == nil then
    self.id = MiniNotify.add(notification)
  else
    MiniNotify.update(self.id, { msg = notification })
  end
end

---Callback function triggered on the CocStatusChange event to handle notifications.
function CocNotify:callback()
  local coc_service_initialized = vim.g.coc_service_initialized
  local coc_status = vim.g.coc_status
  if not (coc_service_initialized or coc_status) then
    return
  end

  -- CoC does not provide a built-in progress tracker for tracking ongoing
  -- operations like LSP `workDoneProgress`. To handle this, we use a
  -- workaround by monitoring changes in `vim.g.coc_status` which contains
  -- status messages from Coc.
  --
  -- Specifically, we look for the string "Loading" in `vim.g.coc_status` to
  -- determine if there is ongoing work. FYI, this approach relies on the
  -- assumption that "Loading" in the status message reliably represents
  -- progress, regardless of the server behind CoC.
  --
  if coc_status:find("Loading") then
    self.indexing = true
    self:update(coc_status)
  elseif self.indexing then
    vim.api.nvim_del_autocmd(self.autocmd)
    vim.schedule(MiniNotify.clear)
  end
end

---@type LazyPluginSpec
local Spec = {
  "mini.notify", dev = true, event = "VeryLazy",

  opts = {
    ERROR = {
      duration = 10000,
    },
  },

  config = function(_, opts)
    local mini_notify = require("mini.notify")
    mini_notify.setup(opts)
    local coc_notify = CocNotify:new()
    coc_notify:setup()
  end,
}

return Spec
