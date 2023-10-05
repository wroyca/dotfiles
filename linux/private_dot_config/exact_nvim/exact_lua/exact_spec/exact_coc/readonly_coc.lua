---@type LazyPluginSpec
return {
  [[coc]],
  event = [[VeryLazy]],
  build = [[yarn install --frozen-lockfile]],

  init = function()
    vim.g.coc_global_extensions = {
      [[coc-marketplace]],
      [[coc-json]],
      [[coc-lua]],
      [[coc-xml]],
      [[coc-clangd]],
      [[coc-clang-format-style-options]]
    }
  end,

  config = function()
    function _G.show_docs ()
      local cw = vim.fn.expand([[<cword>]])
      if vim.fn.index({[[vim]], [[help]]}, vim.bo.filetype) >= 0 then
        vim.api.nvim_command([[h ]] .. cw)
      elseif vim.api.nvim_eval([[coc#rpc#ready()]]) then
        vim.fn.CocActionAsync([[doHover]])
      else
        vim.api.nvim_command([[!]] .. vim.o.keywordprg .. [[ ]] .. cw)
      end
    end

    vim.keymap.set([[i]], [[<Tab>]],      [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]],  { silent = true, noremap = true, expr = true, replace_keycodes = false })
    vim.keymap.set([[n]], [[K]],          [[<cmd>lua _G.show_docs()<cr>]],                      { silent = true })

    vim.keymap.set([[n]], [[gd]],         [[<Plug>(coc-definition)]],                           { silent = true })
    vim.keymap.set([[n]], [[gy]],         [[<Plug>(coc-type-definition)]],                      { silent = true })
    vim.keymap.set([[n]], [[gi]],         [[<Plug>(coc-implementation)]],                       { silent = true })
    vim.keymap.set([[n]], [[gr]],         [[<Plug>(coc-references)]],                           { silent = true })
    vim.keymap.set([[x]], [[<leader>;]],  [[<Plug>(coc-format-selected)]],                      { silent = true })
    vim.keymap.set([[n]], [[<leader>;]],  [[<Plug>(coc-format-selected)]],                      { silent = true })

    vim.keymap.set([[n]], [[<leader>rn]], [[<Plug>(coc-rename)]],                               { silent = true })
    vim.keymap.set([[n]], [[<leader>re]], [[<Plug>(coc-codeaction-refactor)]],                  { silent = true })
    vim.keymap.set([[n]], [[<leader>ac]], [[<Plug>(coc-codeaction-cursor)]],                    { silent = true, nowait = true })

    vim.keymap.set([[x]], [[if]],         [[<Plug>(coc-funcobj-i)]],                            { silent = true, nowait = true })
    vim.keymap.set([[o]], [[if]],         [[<Plug>(coc-funcobj-i)]],                            { silent = true, nowait = true })
    vim.keymap.set([[x]], [[af]],         [[<Plug>(coc-funcobj-a)]],                            { silent = true, nowait = true })
    vim.keymap.set([[o]], [[af]],         [[<Plug>(coc-funcobj-a)]],                            { silent = true, nowait = true })
    vim.keymap.set([[x]], [[ic]],         [[<Plug>(coc-classobj-i)]],                           { silent = true, nowait = true })
    vim.keymap.set([[o]], [[ic]],         [[<Plug>(coc-classobj-i)]],                           { silent = true, nowait = true })
    vim.keymap.set([[x]], [[ac]],         [[<Plug>(coc-classobj-a)]],                           { silent = true, nowait = true })
    vim.keymap.set([[o]], [[ac]],         [[<Plug>(coc-classobj-a)]],                           { silent = true, nowait = true })

    vim.keymap.set([[n]], [[<space>ca]],  [[:<C-u>CocList diagnostics<cr>]],                    { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>ce]],  [[:<C-u>CocList extensions<cr>]],                     { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>cm]],  [[:<C-u>CocList marketplace<cr>]],                    { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>cc]],  [[:<C-u>CocList commands<cr>]],                       { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>co]],  [[:<C-u>CocList outline<cr>]],                        { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>cs]],  [[:<C-u>CocList -I symbols<cr>]],                     { silent = true, nowait = true })
    vim.keymap.set([[n]], [[<space>cp]],  [[:<C-u>CocListResume<cr>]],                          { silent = true, nowait = true })
  end
}
