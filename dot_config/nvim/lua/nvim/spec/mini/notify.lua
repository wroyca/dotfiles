---@module "mini.notify"

---@class CocStatusHandler
---@field coc_notify_id number|nil The ID of the existing notification (if any).
---@field coc_work_in_progress boolean Tracks if a loading process is ongoing.
local CocStatusHandler = {}
CocStatusHandler.__index = CocStatusHandler

--- Constructor for CocStatusHandler.
--- @return CocStatusHandler instance A new instance of the CocStatusHandler class.
function CocStatusHandler:new()
  return setmetatable({
    coc_notify_id = nil,
    coc_work_in_progress = false,
  }, CocStatusHandler)
end

---Sanitizes the Coc status string by removing duplicate words.
---@param status string The original status string from Coc.
---@return string sanitized_status The sanitized status string with duplicates removed.
function CocStatusHandler:sanitize_status(status)
  local seen_words = {}
  local sanitized_status = status:gsub("%S+", function(word)
    if seen_words[word] then
      return ""
    else
      seen_words[word] = true
      return word
    end
  end)
  return sanitized_status:match("^(.-%s%s)")
end

---Updates or creates a notification with the sanitized Coc status.
---@param status string The sanitized status string to be displayed.
function CocStatusHandler:update_notify(status)
  local sanitized_status = self:sanitize_status(status)
  if sanitized_status and sanitized_status:find("stylua") then
    sanitized_status = sanitized_status:gsub("stylua", "")
  end

  if self.coc_notify_id == nil then
    self.coc_notify_id = MiniNotify.add(sanitized_status)
  else
    MiniNotify.update(self.coc_notify_id, { msg = sanitized_status })
  end
end

---Callback function triggered on CocStatusChange to handle notifications.
function CocStatusHandler:on_coc_status_change()
  if not vim.g.coc_service_initialized or not vim.g.coc_status then
    return
  end

  if vim.g.coc_status:find("Loading") then
    self.coc_work_in_progress = true
    self:update_notify(vim.g.coc_status)
  elseif self.coc_work_in_progress then
    vim.api.nvim_del_autocmd(self.coc_status_change_autocmd_id)
    vim.schedule(MiniNotify.clear)
  end
end

---Sets up an autocmd to track CocStatusChange events.
---@param opts table Options table containing configuration settings.
function CocStatusHandler:setup_coc_status_autocmd()
  self.coc_status_change_autocmd_id = vim.api.nvim_create_autocmd("User", {
    pattern = { "CocStatusChange" },
    callback = function()
      self:on_coc_status_change()
    end,
  })
end

---@type LazyPluginSpec
local Spec = {
  "mini.notify",
  dev = true,
  event = "VeryLazy",

  opts = {
    ERROR = {
      duration = 10000,
    },
  },

  --- Configuration function to set up MiniNotify and Coc status handling.
  --- @param _ any Unused parameter for the plugin specification.
  --- @param opts table Options table passed to MiniNotify setup.
  config = function(_, opts)
    local notify = require("mini.notify")
    notify.setup(opts)
    vim.notify = notify.make_notify(opts)

    local handler = CocStatusHandler:new()
    handler:setup_coc_status_autocmd()
  end,
}

return Spec
