---@type LazyPluginSpec
return {
  [[mini.sessions]],
  event = [[VimEnter]],

  cond = function()
    if not (vim.fn.argc(-1) == 0) then
      return false
    end
    local cmd = { [[git]], [[rev-parse]], [[--is-inside-work-tree]] }
    return vim.system(cmd, { cwd = vim.fn.getcwd() }):wait().stdout == "true\n"
  end,

  config = function()
    local current_working_directory = vim.fn.getcwd()
    local project_folder_name = current_working_directory:match("^.+/(.+)$")

    local close_invalid_buffers = function()
      local buffer_numbers = vim.api.nvim_list_bufs()
      for _, buffer_number in pairs(buffer_numbers) do
        local buffer_type = vim.api.nvim_buf_get_option(buffer_number, [[buftype]])
        local buffer_file_type = vim.api.nvim_buf_get_option(buffer_number, [[filetype]])
        if buffer_type == [[nofile]] or buffer_file_type == [[norg]] then
          vim.api.nvim_buf_delete(buffer_number, { force = true })
        end
      end
    end

    local count_open_file_buffers = function()
      local count = 0
      for _, buffer_number in pairs(vim.api.nvim_list_bufs()) do
        local buffer_name = vim.api.nvim_buf_get_name(buffer_number)
        local buffer_file_type = vim.api.nvim_buf_get_option(buffer_number, [[filetype]])
        if buffer_name ~= [[]] and buffer_file_type ~= [[norg]] then
          count = count + 1
        end
      end
      return count
    end

    local MiniSessions = require [[mini.sessions]]
    MiniSessions.setup({
      autoread = false,
      autowrite = true,
      directory = vim.fn.expand("$HOME") .. "/.cache/nvim/sessions/" .. project_folder_name .. "/",
      file    = "",
      force   = { read = false, write = true, delete = false },
      hooks   = { pre  = { read  = nil, write = close_invalid_buffers, delete = nil }, post = { read = nil, write = nil, delete = nil }, },
      verbose = { read = false, write = true, delete = true },
    })

    vim.api.nvim_create_autocmd([[VimLeavePre]], {
      callback = function()
        close_invalid_buffers()
        local number_of_open_buffers = count_open_file_buffers()
        if number_of_open_buffers > 0 then
          MiniSessions.write(project_folder_name .. [[.nvim]])
        end
      end
    })
  end
}
