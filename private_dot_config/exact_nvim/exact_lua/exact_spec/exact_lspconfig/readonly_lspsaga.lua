---@type LazyPluginSpec
return {
  [[nvimdev/lspsaga.nvim]],
  name = [[lspsaga]],
  event = [[LspAttach]],

  opts = {
    lightbulb = {
      enable = false
    },
    symbol_in_winbar = {
      enable = false
    },
    ui = {
      border = [[double]]
    }
  },

  keys = {
    {
      "[e",
      function()
        require [[lspsaga.diagnostic]]:goto_prev({
          severity = vim.diagnostic.severity.ERROR
        })
      end,
      desc = [[Go to previous diagnostic (ERROR)]]
    },
    {
      "]e",
      function()
        require [[lspsaga.diagnostic]]:goto_next({
          severity = vim.diagnostic.severity.ERROR
        })
      end,
      desc = [[Go to next diagnostic (ERROR)]]
    },
    {
      "[w",
      function()
        require [[lspsaga.diagnostic]]:goto_prev({
          severity = vim.diagnostic.severity.WARN
        })
      end,
      desc = [[Go to previous diagnostic (WARN)]]
    },
    {
      "]w",
      function()
        require [[lspsaga.diagnostic]]:goto_next({
          severity = vim.diagnostic.severity.WARN
        })
      end,
      desc = [[Go to next diagnostic (WARN)]]
    },
    {
      "[i",
      function()
        require [[lspsaga.diagnostic]]:goto_prev({
          severity = vim.diagnostic.severity.INFO
        })
      end,
      desc = [[Go to previous diagnostic (INFO)]]
    },
    {
      "]i",
      function()
        require [[lspsaga.diagnostic]]:goto_next({
          severity = vim.diagnostic.severity.INFO
        })
      end,
      desc = [[Go to next diagnostic (INFO)]]
    },
    {
      "[h",
      function()
        require [[lspsaga.diagnostic]]:goto_prev({
          severity = vim.diagnostic.severity.HINT
        })
      end,
      desc = [[Go to previous diagnostic (HINT)]]
    },
    {
      "]h",
      function()
        require [[lspsaga.diagnostic]]:goto_next({
          severity = vim.diagnostic.severity.HINT
        })
      end,
      desc = [[Go to next diagnostic (HINT)]]
    },
    {
      [[gd]],
      [[<cmd>Lspsaga peek_definition<cr>]],
      desc = [[Peek Definition]]
    },
    {
      [[gD]],
      [[<cmd>Lspsaga goto_definition<cr>]],
      desc = [[Goto Definition]]
    },
    {
      [[gt]],
      [[<cmd>Lspsaga peek_type_definition<cr>]],
      desc = [[Peek Type Definition]]
    },
    {
      [[gT]],
      [[<cmd>Lspsaga goto_type_definition<cr>]],
      desc = [[Goto Type Definition]]
    },
    {
      [[<C-k>]],
      [[<cmd>Lspsaga hover_doc ++quiet<cr>]],
      desc = [[Hover Documentation (Quiet)]]
    },
    {
      [[<C-K>]],
      [[<cmd>Lspsaga hover_doc ++keep<cr>]],
      desc = [[Hover Documentation (Keep)]]
    },
    {
      [[<leader>lo]],
      [[<cmd>Lspsaga outline<cr>]],
      desc = [[Outline]]
    },
    {
      [[<leader>lw]],
      [[<cmd>Lspsaga show_workspace_diagnostics<cr>]],
      desc = [[Show Workspace Diagnostics]]
    },
    -- BUG: crash clangd.
    --
    --{
    --  [[<leader>lc]],
    --  [[<cmd>Lspsaga incoming_calls<cr>]],
    --  desc = [[Incoming Calls]]
    --},
    {
      [[<leader>lC]],
      [[<cmd>Lspsaga outgoing_calls<cr>]],
      desc = [[Outgoing Calls]]
    },
    {
      [[<leader>ld]],
      [[<cmd>Lspsaga show_line_diagnostics<cr>]],
      desc = [[Show Line Diagnostics]]
    },
    {
      [[<leader>lD]],
      [[<cmd>Lspsaga show_buf_diagnostics<cr>]],
      desc = [[Show Buffer Diagnostics]]
    },
    {
      [[<leader>lr]],
      [[<cmd>Lspsaga rename<cr>]],
      desc = [[Rename in Buffer]]
    },
    {
      [[<leader>lR]],
      [[<cmd>Lspsaga rename ++project<cr>]],
      desc = [[Rename in Project]]
    },
    {
      [[<leader>la]],
      [[<cmd>Lspsaga code_action<cr>]],
      desc = [[Code Action]]
    },
    {
      [[<leader>;]],
      function()
        vim.lsp.buf.format {
          async = true
        }
      end
    }
  }
}
