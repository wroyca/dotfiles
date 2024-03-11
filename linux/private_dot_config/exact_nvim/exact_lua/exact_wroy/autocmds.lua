local cache = nil

vim.fn.jobstart({ [[kitty]], [[@]], [[get-colors]] }, {
  on_stdout = function(_, d, _)
    for _, result in ipairs(d) do
      if string.match(result, [[^background]]) then
        cache = vim.split(result, [[%s+]])[2]
        break
      end
    end
  end,
  on_stderr = function(_, d, _)
    if #d > 1 then
      vim.api.nvim_err_writeln(
        [[Error getting background. Make sure kitty remote control is turned on.]]
      )
    end
  end
})

local set = function(color, sync)
	local command = [[kitty @ set-colors background=]] .. color
	if not sync then
		vim.fn.jobstart(command, {
			on_stderr = function(_, d, _)
				if #d > 1 then
					vim.api.nvim_err_writeln(
						[[Error changing background. Make sure kitty remote control is turned on.]]
					)
				end
			end
		})
	else
		vim.fn.system(command)
	end
end

vim.api.nvim_create_autocmd({[[ColorScheme]], [[VimResume]]}, {
  pattern = "*",
  callback = function()
    set(vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.hlID([[Normal]])), [[bg]]))
	end,
})

vim.api.nvim_create_autocmd({[[VimLeavePre]], [[VimSuspend]]}, {
  callback = function()
		set(cache, true)
	end
})
