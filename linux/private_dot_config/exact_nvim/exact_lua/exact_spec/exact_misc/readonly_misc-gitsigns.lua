return {
  [[misc-gitsigns]],
  ft = { [[gitcommit]], [[diff]] },
  init = function()
    vim.api.nvim_create_autocmd({ [[BufRead]] }, {
      group = vim.api.nvim_create_augroup([[GitSignsLazyLoad]], { clear = true }),
      callback = function()
        vim.fn.system([[git -C ]] .. [["]] .. vim.fn.expand([[%:p:h]]) .. [["]] .. [[ rev-parse]])
        if vim.v.shell_error == 0 then
          vim.api.nvim_del_augroup_by_name([[GitSignsLazyLoad]])
          vim.schedule(function()
            require [[lazy]].load({ plugins = { [[misc-gitsigns]] } })
          end)
        end
      end
    })
  end,
  opts = {
    signs = {
      add = {
        text = [[┃]]
      },
      change = {
        text = [[┃]]
      },
      delete = {
        text = [[┃]]
      },
      topdelete = {
        text = [[┃]]
      },
      changedelete = {
        text = [[┃]]
      }
    }
  }
}
