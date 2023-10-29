---@type LazyPluginSpec
return {
  [[hrsh7th/nvim-cmp]],
  name = [[cmp]],
  event = [[InsertEnter]],

  opts = function()
    local cmp = require [[cmp]]
    return {
      --- List completion sources and their priority.
      sources = cmp.config.sources({
        { name = [[nvim_lsp]] },
        { name = [[nvim_lsp_document_symbol]] },
        { name = [[nvim_lsp_signature_help]] }
      }),

      --- https://github.com/neovim/neovim/pull/25301
      snippet = {
        expand = function(args)
          vim.snippet.expand(args.body)
        end
      },

      mapping = cmp.mapping.preset.insert({
        ['<Tab>'] = cmp.mapping.confirm({
          select = true
        })
      }),

      formatting = {
        fields = {
          [[abbr]],
          [[kind]],
          [[menu]]
        },

        format = function(entry, vim_item)
          pcall(function()
            local completion_item = entry:get_completion_item() --[[@as lsp.CompletionItem]]
            if completion_item.detail then
              vim_item.detail = completion_item.detail
            end
          end)
          return vim_item
        end
      }
    }
  end
}
