return {
    'tpope/vim-fugitive',
    dependencies = {
      'folke/which-key.nvim',
    },
    config = function ()
      local wk = require('which-key')

      wk.register({
        g = {
          name = '+git',
          s = { '<cmd>Git<cr>', 'git [s]tatus' },
          g = { ':Git<space>', '[G]it' },
        }
      }, { prefix = '<leader>', silent = false })
    end
}
