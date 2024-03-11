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

    -- https://github.com/neovim/neovim/issues/24252
    --
    vim.keymap.set([[i]], [[<Tab>]],     [[coc#pum#visible() ? coc#pum#confirm() : "<Tab>"]],  {                                   silent = true, noremap = true, expr = true, replace_keycodes = false })
    vim.keymap.set([[n]], [[K]],         [[<cmd>lua _G.show_docs()<cr>]],                      {                                   silent = true                                                        })
    vim.keymap.set([[n]], [[gd]],        [[<Plug>(coc-definition)]],                           { desc = [[Go to definition]],      silent = true                                                        })
    vim.keymap.set([[n]], [[gD]],        [[<Plug>(coc-type-definition)]],                      { desc = [[Go to type definition]], silent = true                                                        })
    vim.keymap.set([[n]], [[gi]],        [[<Plug>(coc-implementation)]],                       { desc = [[Go to implementation]],  silent = true                                                        })
    vim.keymap.set([[n]], [[gr]],        [[<Plug>(coc-references)]],                           { desc = [[Go to references]],      silent = true                                                        })
    vim.keymap.set([[x]], [[<leader>;]], [[<Plug>(coc-format-selected)]],                      { desc = [[Format selection]],      silent = true                                                        })
    vim.keymap.set([[n]], [[<leader>;]], [[<Plug>(coc-format-selected)]],                      { desc = [[Format selection]],      silent = true                                                        })
    vim.keymap.set([[n]], [[<leader>.]], [[<Plug>(coc-codeaction-cursor)]],                    { desc = [[Code action]],           silent = true, nowait = true                                         })
    vim.keymap.set([[x]], [[if]],        [[<Plug>(coc-funcobj-i)]],                            {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[o]], [[if]],        [[<Plug>(coc-funcobj-i)]],                            {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[x]], [[af]],        [[<Plug>(coc-funcobj-a)]],                            {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[o]], [[af]],        [[<Plug>(coc-funcobj-a)]],                            {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[x]], [[ic]],        [[<Plug>(coc-classobj-i)]],                           {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[o]], [[ic]],        [[<Plug>(coc-classobj-i)]],                           {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[x]], [[ac]],        [[<Plug>(coc-classobj-a)]],                           {                                   silent = true, nowait = true                                         })
    vim.keymap.set([[o]], [[ac]],        [[<Plug>(coc-classobj-a)]],                           {                                   silent = true, nowait = true                                         })
  end
}
