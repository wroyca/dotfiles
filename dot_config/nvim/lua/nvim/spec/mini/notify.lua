---@module "mini.notify"

---@type LazyPluginSpec
local Spec = {
  "mini.notify", dev = true, event = "VeryLazy",

  opts = {
    ERROR = {
      duration = 10000
    }
  },

  config = function(_, opts)
    local notify = require "mini.notify"
    notify.setup (opts)
    vim.notify = notify.make_notify (opts)

    COC_STATUS_CHANGE = 0
    COC_WORK_DONE_PROGRESS = false
    COC_STATUS_CHANGE = vim.api.nvim_create_autocmd ("User", {
      pattern = { "CocStatusChange" },
      callback = function ()
        if not vim.g.coc_service_initialized then return end
        if not vim.g.coc_status then return end

        -- Workaround for missing LSP progress tracking in Coc
        --
        -- Coc does not provide a built-in progress tracker for tracking
        -- ongoing operations like LSP `workDoneProgress`. To handle this, we
        -- use a workaround by monitoring changes in `vim.g.coc_status` which
        -- contains status messages from Coc.
        --
        -- Specifically, we look for the presence of the string "Loading" in
        -- `vim.g.coc_status` to determine if there is ongoing work that should
        -- be tracked. When we detect "Loading", we assume that some work is in
        -- progress and create or update a notification to reflect this. Once
        -- the status message changes and no longer indicates "Loading", we
        -- assume the work has completed and clear the notification.
        --
        -- FYI, this approach relies on the assumption that "Loading" in the
        -- status message reliably represents progress, regardless of the
        -- server behind Coc, so good luck with that. ;)
        --
        if string.find(vim.g.coc_status, "Loading") then
          COC_WORK_DONE_PROGRESS = true

          -- Thought the previous workaround was over the top? Wait until you
          -- see this next;
          --
          -- Recall our earlier discussion about "Loading"? It turns out
          -- vim.g.coc_status also adds seemingly unrelated information
          -- alongside the LSP progress. While we only want to track LSP
          -- progress, coc_status includes details like linter and formatter
          -- names. Although this behavior might seem excessive, it makes sense
          -- for a statusline, which benefits from such information. We’re
          -- essentially exploiting this feature.
          --
          -- Nonetheless, we need to clean up the input before forwarding it to
          -- mini.notify. Specifically, we have two issues to address: first,
          -- we need to remove any unrelated identifiers (e.g., stylua).
          -- Second, we must deduplicate the input. Why deduplicate? Some LSPs
          -- send two workspace/configuration requests to the client: one with
          -- a scopeUri for the workspace folder and another without it,
          -- intended for fallback settings. Unfortunately, some LSP
          -- implementations, including Coc, ignore the scopeUri and provide
          -- identical settings in both cases, leading to duplicate progress
          -- bars.
          --
          -- Handling this with a proper config.handlers hook would be ideal,
          -- but in the absence of a built-in progress tracker, our best option
          -- is to parse vim.g.status...
          local sanitize = function (input)
            -- Table to track encountered words.
            local dup = {}
            local out = input:gsub ("%S+", function (word)
              if dup[word] then return "" end
              dup[word] = true
            end)
            return out:match  "^(.-%s%s)"
          end
          local coc_status = sanitize(vim.g.coc_status)
          if string.find(coc_status, "stylua") then coc_status = coc_status:gsub("stylua", "") end

          if COC_NOTIFY == nil then
            COC_NOTIFY = MiniNotify.add (coc_status)
            return
          else
            return MiniNotify.update (COC_NOTIFY, { msg = coc_status })
          end
        end
        if COC_WORK_DONE_PROGRESS then
          vim.api.nvim_del_autocmd(COC_STATUS_CHANGE)
          vim.schedule(function ()
            MiniNotify.clear()
          end)
        end
      end
    })
  end
}

return Spec
