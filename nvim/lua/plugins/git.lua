return {
  {
    "kdheepak/lazygit.nvim",
    cmd = {
      "LazyGit",
      "LazyGitConfig",
      "LazyGitCurrentFile",
      "LazyGitFilter",
      "LazyGitFilterCurrentFile",
    },
    -- optional for floating window border decoration
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    -- setting the keybinding for LazyGit with 'keys' is recommended in
    -- order to load the plugin when the command is run for the first time
    keys = {
      { "<leader>g", "<cmd>LazyGit<cr>", desc = "LazyGit" }
    }
  },
  {
    'tpope/vim-fugitive',
    dependencies = {
      'folke/which-key.nvim',
      {
        'lewis6991/gitsigns.nvim',
        config = function()
          require('gitsigns').setup({
            current_line_blame = true,
            on_attach = function(bufnr)
              local gitsigns = require('gitsigns')

              local function map(mode, l, r, opts)
                opts = opts or {}
                opts.buffer = bufnr
                vim.keymap.set(mode, l, r, opts)
              end

              map('n', ']c', function()
                if vim.wo.diff then
                  vim.cmd.normal({ ']c', bang = true })
                else
                  gitsigns.nav_hunk('next')
                end
              end)

              map('n', '[c', function()
                if vim.wo.diff then
                  vim.cmd.normal({ '[c', bang = true })
                else
                  gitsigns.nav_hunk('prev')
                end
              end)
            end
          })
        end
      }
    },
  },
}
