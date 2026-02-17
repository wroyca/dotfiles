---@module "mini.bufremove"

---@type LazyPluginSpec
local Spec = {
  "mini.bufremove", virtual = true,

  keys = {
    {
      "<leader>bd",
      function ()
        MiniBufremove.delete (0, false)
      end,
      desc = "Delete",
    },
    {
      "<leader>bw",
      function ()
        MiniBufremove.wipeout (0, false)
      end,
      desc = "Wipe",
    },
    {
      "<leader>bu",
      function ()
        MiniBufremove.unshow (0)
      end,
      desc = "Hide",
    },
  },

  config = function (_, opts)
    require ("mini.bufremove").setup (opts)

    -- Override the default wipeout logic to enforce stricter cleanup.
    --
    -- As noted in the keymap description, we expect a wipeout to remove
    -- all traces of the buffer. The default implementation leaves the file
    -- path in the persistent history (`v:oldfiles` and shada), which
    -- contradicts the intent of a full wipe.
    --
    -- We wrap the function to handle the history scrubbing manually after
    -- the buffer is detached.
    --
    local original_wipeout = MiniBufremove.wipeout

    ---@diagnostic disable-next-line: duplicate-set-field
    MiniBufremove.wipeout = function (buf_id, force)
      -- We must capture the buffer name before handing control to the
      -- original wipeout function, as the buffer object will be invalid
      -- immediately after.
      --
      local buf = buf_id or vim.api.nvim_get_current_buf ()
      local bufname = vim.api.nvim_buf_get_name (buf)

      original_wipeout (buf, force)

      if bufname ~= "" then
        -- This looks aggressive, but it is the only way to ensure consistency.
        -- We force a read-merge of the shada file, filter the memory list,
        -- and force a write-back. This ensures that if we "Wipe" a file,
        -- it doesn't reappear in the dashboard or oldfiles list on the
        -- next startup.
        --
        pcall (vim.cmd.wshada, { bang = true })
        vim.v.oldfiles = vim.tbl_filter (function (file)
          return file ~= bufname
        end, vim.v.oldfiles)
        pcall (vim.cmd.wshada, { bang = true })
      end
    end
  end,
}

return Spec
