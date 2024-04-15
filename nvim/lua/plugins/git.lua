return {
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
    config = function()
      local wk = require('which-key')

      wk.register({
        g = { ":Git ", "fu[git]ive" }
      }, { prefix = '<leader>', silent = false })
    end
  },
}
