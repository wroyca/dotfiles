---@module "mini.files"

---@class MiniFilesEntry
---@field name string Entry name
---@field path string Full path
---@field fs_type string Entry type ('file' or 'directory')

---@class MiniFilesGitignore
---@field private cache table<string, table<string, boolean>> Directory to ignored files mapping. Key is directory path, value is table mapping file paths to ignored status
---@field private mtimes table<string, number> Directory to .gitignore mtime mapping. Key is directory path, value is last modified timestamp
---@field private state boolean Current filtering state (true = show all, false = respect gitignore)
---@field private initial_sort function Original sort function from options
---@field private initial_filter function Original filter function from options
---@field private current_sort function Current active sort function
---@field private current_filter function Current active filter function
---@field private notified table<string, boolean> Track directories that have already shown gitignore change notifications
local MiniFilesGitignore = {}

---Initialize new MiniFilesGitignore instance
---@param opts MiniFilesSpec Configuration options
---@return MiniFilesGitignore
function MiniFilesGitignore.new (opts)
  local self = setmetatable ({}, { __index = MiniFilesGitignore })
  self.cache = {}
  self.mtimes = {}
  self.state = false -- Start with gitignore filtering enabled
  self.initial_sort = opts.content.sort
  self.initial_filter = opts.content.filter
  self.current_sort = self.initial_sort
  self.current_filter = self.initial_filter
  self.notified = {}
  return self
end

---Get current sort function
---@return function sort Current sort function
function MiniFilesGitignore:get_sort ()
  return self.current_sort
end

---Get current filter function
---@return function filter Current filter function
function MiniFilesGitignore:get_filter ()
  return self.current_filter
end

---Check if directory's gitignore patterns are cached
---@param dir string Absolute path to directory
---@return boolean true if directory has cached gitignore results
---@private
function MiniFilesGitignore:is_cached (dir)
  local cached = self.cache[dir] ~= nil
  return cached
end

---Check if file is ignored by gitignore rules
---@param dir string Absolute path to directory containing file
---@param file string Absolute path to file to check
---@return boolean true if file is ignored by gitignore rules
---@private
function MiniFilesGitignore:is_ignored (dir, file)
  local ignored = self.cache[dir] and self.cache[dir][file] or false
  return ignored
end

---Cache gitignore results for directory
---@param dir string Absolute path to directory
---@param ignored_files string[] List of absolute paths to ignored files
---@private
function MiniFilesGitignore:cache_results (dir, ignored_files)
  self.cache[dir] = self.cache[dir] or {}
  for _, file in ipairs (ignored_files) do
    self.cache[dir][file] = true
  end

  -- Update .gitignore mtime
  local gitignore_path = vim.fn.fnamemodify (dir .. "/.gitignore", ":p")
  if vim.fn.filereadable (gitignore_path) == 1 then
    self.mtimes[dir] = vim.fn.getftime (gitignore_path)
  end

  -- Schedule immediate refresh after caching
  vim.schedule (function ()
    if not MiniFiles.is_busy then
      MiniFiles.refresh ({
        content = {
          sort = self.current_sort,
          filter = self.current_filter,
        },
      })
    end
  end)
end

---Check if .gitignore has changed in directory
---@param dir string Absolute path to directory
---@param gitignore_path string Absolute path to .gitignore file
---@return boolean changed Whether .gitignore file has changed
---@return number? mtime Modified timestamp of .gitignore if file exists
---@private
function MiniFilesGitignore:has_changed (dir, gitignore_path)
  if vim.fn.filereadable (gitignore_path) ~= 1 then
    return false
  end

  local mtime = vim.fn.getftime (gitignore_path)
  local changed = not self.mtimes[dir] or self.mtimes[dir] ~= mtime
  return changed, mtime
end

---Invalidate cache for directory
---@param dir string Absolute path to directory
---@private
function MiniFilesGitignore:invalidate (dir)
  self.cache[dir] = nil
  self.mtimes[dir] = nil
  self.notified[dir] = nil -- Reset notification state when invalidating
end

---Clear all caches
---@private
function MiniFilesGitignore:clear ()
  self.cache = {}
  self.mtimes = {}
  self.notified = {} -- Clear notification tracking
end

---Process uncached directories asynchronously
---@param uncached_dirs string[] List of directories needing gitignore processing
---@param fs_entries MiniFilesEntry[] List of filesystem entries
---@private
function MiniFilesGitignore:process_uncached_dirs (uncached_dirs, fs_entries)
  if #uncached_dirs == 0 then
    return
  end

  -- Queue to track running jobs
  local queue = {
    jobs = {},
    coro = nil, -- coroutine for async job management
    add = function (queue, dir, id)
      table.insert (queue.jobs, { dir = dir, id = id })
    end,
    remove = function (queue, id)
      for i, job in ipairs (queue.jobs) do
        if job.id == id then
          table.remove (queue.jobs, i)
          break
        end
      end
      -- Resume when all jobs complete
      if #queue.jobs == 0 and queue.coro then
        coroutine.resume (queue.coro)
      end
    end,
    is_empty = function (queue)
      local empty = #queue.jobs == 0
      return empty
    end,
    cancel_all = function (queue)
      for _, job in ipairs (queue.jobs) do
        pcall (vim.fn.jobstop, job.id)
      end
      queue.jobs = {}
    end,
  }

  -- Create coroutine to manage async processing
  local co = coroutine.create (function ()
    -- Process each directory
    vim.iter (uncached_dirs):each (function (dir)
      -- Use jobstart for non-blocking gitignore checks
      local job_id
      local function handle_job_complete ()
        queue:remove (job_id)
      end

      -- Run git check-ignore in the directory being checked
      job_id = vim.fn.jobstart ({ "git", "-C", dir, "check-ignore", "--stdin" }, {
        stdout_buffered = true,
        on_stdout = function (_, files)
          self:cache_results (dir, files)
        end,
        on_exit = function (_, code)
          handle_job_complete ()
        end,
      })

      queue:add (dir, job_id)

      -- Batch file checks per directory
      local files_to_check = vim
        .iter (fs_entries)
        :filter (function (entry)
          return vim.fn.fnamemodify (entry.path, ":h") == dir
        end)
        :map (function (entry)
          return entry.path
        end)
        :totable ()

      vim.fn.chansend (job_id, table.concat (files_to_check, "\n"))
      vim.fn.chanclose (job_id, "stdin")
    end)

    -- Handle empty queue case (all jobs failed to start)
    if queue:is_empty () then
      return
    end

    -- Yield until all jobs complete
    coroutine.yield ()

    -- Schedule final refresh after all jobs complete
    vim.schedule (function ()
      if not MiniFiles.is_busy then
        MiniFiles.refresh ({
          content = {
            sort = self.current_sort,
            filter = self.current_filter,
          },
        })
      end
    end)
  end)

  -- Store coroutine reference in queue
  queue.coro = co

  -- Start the coroutine with error handling
  local ok, err = coroutine.resume (co)
  if not ok then
    vim.notify (string.format ("Error processing gitignore: %s", err), vim.log.levels.ERROR)
  end
end

---Custom sort function that handles gitignore integration
---@param fs_entries MiniFilesEntry[] List of filesystem entries
---@return MiniFilesEntry[] Sorted and filtered entries
function MiniFilesGitignore:sort_entries (fs_entries)
  -- Skip gitignore processing if in unfiltered state
  if self.state then
    return MiniFiles.default_sort (fs_entries)
  end

  local uncached_dirs = {}
  local processed_dirs = {} -- prevent duplicate processing

  -- Process changes and collect uncached dirs
  vim.iter (fs_entries):each (function (entry)
    local dir = vim.fn.fnamemodify (entry.path, ":h")

    -- Skip if we've already processed this directory
    if processed_dirs[dir] then
      return
    end
    processed_dirs[dir] = true

    local gitignore_path = vim.fn.fnamemodify (dir .. "/.gitignore", ":p")

    if self:has_changed (dir, gitignore_path) then
      self.cache[dir] = nil
      self.mtimes[dir] = nil

      -- Show notification only if not already notified
      if not self.notified[dir] then
        self.notified[dir] = true
      end
    end

    -- Batch uncached directories
    if not self:is_cached (dir) and not vim.tbl_contains (uncached_dirs, dir) then
      table.insert (uncached_dirs, dir)
    end
  end)

  -- Process uncached directories asynchronously
  self:process_uncached_dirs (uncached_dirs, fs_entries)

  -- Apply filtering and sorting
  return MiniFiles.default_sort (vim.tbl_filter (function (entry)
    local dir = vim.fn.fnamemodify (entry.path, ":h")
    return not self:is_ignored (dir, entry.path)
  end, fs_entries))
end

---Toggle between filtered and unfiltered views
function MiniFilesGitignore:toggle_filtering ()
  self.state = not self.state

  -- Only validate gitignore in filtered mode
  if not self.state then
    self:validate_gitignore ()
  end

  -- Switch between filtered/unfiltered modes
  self.current_sort = self.state and MiniFiles.default_sort or self.initial_sort
  self.current_filter = self.state and MiniFiles.default_filter or self.initial_filter

  self:force_refresh ()
end

---Validate gitignore status for current buffer
---@private
function MiniFilesGitignore:validate_gitignore ()
  local buf = vim.api.nvim_get_current_buf ()
  local lines = vim.api.nvim_buf_get_lines (buf, 0, -1, false)
  local needs_refresh = false

  -- Selective cache invalidation based on actual changes
  vim
    .iter (lines)
    :filter (function (line)
      return line ~= ""
    end)
    :each (function (line)
      local path = line:match ("^%s*(.-)%s*$")
      if path then
        local dir = vim.fn.fnamemodify (path, ":h")
        local gitignore_path = vim.fn.fnamemodify (dir .. "/.gitignore", ":p")

        if self:has_changed (dir, gitignore_path) then
          self:invalidate (dir)
          needs_refresh = true
        end
      end
    end)

  -- Fallback to full cache clear for edge cases
  if not needs_refresh then
    local current_dir = vim.fn.expand ("%:p:h")
    local gitignore_path = vim.fn.fnamemodify (current_dir .. "/.gitignore", ":p")
    if self:has_changed (current_dir, gitignore_path) then
      self:clear ()
    end
  end
end

---Force refresh file view
---@private
function MiniFilesGitignore:force_refresh ()
  local function refresh_with_dummy_filter (char)
    MiniFiles.refresh ({
      content = {
        filter = function (fs_entry)
          return not vim.startswith (fs_entry.name, char)
        end,
      },
    })
  end

  refresh_with_dummy_filter (";")
  refresh_with_dummy_filter (".")

  MiniFiles.refresh ({
    content = {
      sort = self.current_sort,
      filter = self.current_filter,
    },
  })
end

---@class MiniFilesSpec
---@field content table Configuration for file content display
---@field content.sort function Custom sort function for filesystem entries
---@field content.filter function Custom filter function for filesystem entries
---@field windows table Window display configuration
---@field windows.max_number number Maximum number of windows to show
---@field options table General plugin options
---@field options.permanent_delete boolean Whether to permanently delete files
---@type LazyPluginSpec
local Spec = {
  "mini.files", virtual = true,

  keys = {
    {
      "<leader>f",
      function ()
        local file = vim.api.nvim_buf_get_name (0)
        local file_exists = vim.fn.filereadable (file) ~= 0
        MiniFiles.open (file_exists and file or nil)
        MiniFiles.reveal_cwd ()
        MiniFiles.refresh ({
          content = {
            sort = MiniFiles.sort,
            filter = MiniFiles.filter,
          },
        })
      end,
      desc = "Files",
    },
  },

  opts = {
    content = {
      ---Custom sort function that handles gitignore integration
      ---@param fs_entries MiniFilesEntry[] List of filesystem entries
      ---@return MiniFilesEntry[] Sorted and filtered entries
      sort = function (fs_entries)
        return MiniFiles.gitignore:sort_entries (fs_entries)
      end,

      ---Default filter to hide dotfiles
      ---@param fs_entry MiniFilesEntry Entry to check
      ---@return boolean true if entry should be shown
      filter = function (fs_entry)
        return not vim.startswith (fs_entry.name, ".")
      end,
    },

    windows = { max_number = 1 },
    options = { permanent_delete = false },
  },

  ---Plugin setup function
  ---@param _ any Unused
  ---@param opts MiniFilesSpec Configuration options
  config = function (_, opts)
    require ("mini.files").setup (opts)

    MiniFiles.gitignore = MiniFilesGitignore.new (opts)
    vim.api.nvim_create_autocmd ("User", {
      pattern = "MiniFilesBufferCreate",
      callback = function (args)
        vim.keymap.set ("n", ".", function ()
          MiniFiles.gitignore:toggle_filtering ()
        end, { buffer = args.data.buf_id })
      end,
    })
  end,
}

return Spec
