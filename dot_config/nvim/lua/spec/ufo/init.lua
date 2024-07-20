return {
  [[kevinhwang91/nvim-ufo]], name = [[ufo]], event = [[VeryLazy]],

  init = function()
    vim.o.foldlevel = 99
    vim.o.foldlevelstart = 99
  end,

  opts = {
    provider_selector = function(_, filetype, buftype)
      local function handleFallbackException(bufnr, err, providerName)
        if type(err) == [[string]] and err:match([[UfoFallbackException]]) then
          return require [[ufo]].getFolds(bufnr, providerName)
        else
          return require [[promise]].reject(err)
        end
      end
      return (filetype == [[]] or buftype == [[nofile]]) and [[indent]] or function(bufnr)
        return require [[ufo]].getFolds(bufnr, [[lsp]]):catch(function(err)
          return handleFallbackException(bufnr, err, [[treesitter]])
        end):catch(function(err)
          return handleFallbackException(bufnr, err, [[indent]])
        end)
      end
    end
  },

  config = function (_, opts)
    require'ufo'.setup({
      enable_get_fold_virt_text = true,
      fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate, ctx)
        -- include the bottom line in folded text for additional context
        local filling = ' ⋯ '
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        table.insert(virtText, {filling, 'Folded'})
        local endVirtText = ctx.get_fold_virt_text(endLnum)
        for i, chunk in ipairs(endVirtText) do
          local chunkText = chunk[1]
          local hlGroup = chunk[2]
          if i == 1 then
            chunkText = chunkText:gsub("^%s+", "")
          end
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(virtText, {chunkText, hlGroup})
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            table.insert(virtText, {chunkText, hlGroup})
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            -- str width returned from truncate() may less than 2nd argument, need padding
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        return virtText
      end,
    })
  end
}
