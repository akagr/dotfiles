return {
  {
    'tpope/vim-fugitive',
    dependencies = {
      'folke/which-key.nvim'
    },
    config = function()
      local wk = require('which-key')

      wk.register({
        g = { ":Git ", "fu[git]ive" }
      }, { prefix = '<leader>', silent = false })
    end
  },
  {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup({
        current_line_blame = true,
      })
    end
  }
}
