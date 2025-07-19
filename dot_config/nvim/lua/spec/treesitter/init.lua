---@module "treesitter"

---@type LazyPluginSpec
local Spec = {
  "nvim-treesitter/nvim-treesitter",
  branch = "main",
  build = ":TSUpdate",
  event = "User LazyFile",

  opts = {
    install_dir = vim.fs.joinpath (
      vim.fn.stdpath ("data"),
      "..",
      "chezmoi",
      "dot_config",
      "nvim"
    ),

    -- Treesitter grammars that are required by Neovim's runtime. These are
    -- mandatory and must not be removed.
    grammars = {
      "c",
      "lua",
      "vim",
      "vimdoc",
      "query",
      "markdown",
      "markdown_inline",
    },

    -- Additional grammars we wish to support, but which are not strictly
    -- required.
    --
    -- Note: care must be taken when adding auxiliary grammars that are
    -- designed to operate *in parallel* with primary language grammars. When
    -- active, these may attach concurrently and introduce performance overhead
    -- and/or conflicting syntax nodes.
    --
    -- For example, "comment" alongside "cpp" result in degraded performance on
    -- large C++ files with extensive comment blocks.
    optional_grammars = {
      "cpp",
    },
  },

  config = function (_, opts)
    require ("nvim-treesitter").setup (opts)

    local function not_installed (lang)
      return vim.fn.empty (
        vim.fn.glob (opts.install_dir .. "/" .. lang .. ".*")
      ) == 1
    end

    local ensure_installed = vim.list_extend (
      vim.tbl_extend ("force", {}, opts.grammars),
      opts.optional_grammars
    )
    local missing = vim.tbl_filter (not_installed, ensure_installed)

    if #missing > 0 then
      require ("nvim-treesitter").install (missing)
    end

    vim.api.nvim_create_autocmd ("FileType", {
      pattern = vim
        .iter (ensure_installed)
        :map (vim.treesitter.language.get_filetypes)
        :flatten ()
        :totable (),
      callback = function (ev)
        vim.treesitter.start (ev.buf)
      end,
    })
  end,
}

vim.opt.runtimepath:append (Spec.opts.install_dir)
return Spec
