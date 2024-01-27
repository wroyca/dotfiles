---@type LazyPluginSpec
return {
  [[lspconfig]],
  config = function()
    vim.keymap.set([[n]], [[<space>lf]], vim.diagnostic.open_float, { desc = [[Diagnostic]] })
    vim.keymap.set([[n]], [[<space>ll]], vim.diagnostic.setloclist, { desc = [[Location list]] })

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    vim.api.nvim_create_autocmd([[LspAttach]], {
      group = vim.api.nvim_create_augroup([[UserLspConfig]], {}),
      callback = function(ev)
        vim.keymap.set({ [[n]]        }, [[<space>lD]],                                      vim.lsp.buf.declaration,                    { buffer = ev.buf, desc = [[Declaration]] })
        vim.keymap.set({ [[n]]        }, [[<space>ld]],                                      vim.lsp.buf.definition,                     { buffer = ev.buf, desc = [[Definition]] })
        vim.keymap.set({ [[n]]        }, [[<space>lh]],                                      vim.lsp.buf.hover,                          { buffer = ev.buf, desc = [[Hover]] })
        vim.keymap.set({ [[n]]        }, [[<space>lI]],                                      vim.lsp.buf.implementation,                 { buffer = ev.buf, desc = [[Implementation]] })
        vim.keymap.set({ [[n]]        }, [[<space>ls]],                                      vim.lsp.buf.signature_help,                 { buffer = ev.buf, desc = [[Signature help]] })
        vim.keymap.set({ [[n]]        }, [[<space>lwa]],                                     vim.lsp.buf.add_workspace_folder,           { buffer = ev.buf, desc = [[Add]] })
        vim.keymap.set({ [[n]]        }, [[<space>lwr]],                                     vim.lsp.buf.remove_workspace_folder,        { buffer = ev.buf, desc = [[Remove]] })
        vim.keymap.set({ [[n]]        }, [[<space>lwl]],        function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, { buffer = ev.buf, desc = [[List]] })
        vim.keymap.set({ [[n]]        }, [[<space>lt]],                                      vim.lsp.buf.type_definition,                { buffer = ev.buf, desc = [[Type definition]] })
        vim.keymap.set({ [[n]]        }, [[<space>lr]],                                      vim.lsp.buf.rename,                         { buffer = ev.buf, desc = [[Rename]] })
        vim.keymap.set({ [[n]], [[v]] }, [[<space>lc]],                                      vim.lsp.buf.code_action,                    { buffer = ev.buf, desc = [[Code action]] })
        vim.keymap.set({ [[n]]        }, [[<space>lR]],                                      vim.lsp.buf.references,                     { buffer = ev.buf, desc = [[Reference]] })
        vim.keymap.set({ [[n]], [[v]] }, [[<space>;]],                           function()  vim.lsp.buf.format { async = true } end,    { buffer = ev.buf, desc = [[Format]] })
      end
    })
  end
}
