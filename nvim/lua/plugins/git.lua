return {
    'tpope/vim-fugitive',
    dependencies = {
      'folke/which-key.nvim',
    },
    config = function ()
      local wk = require('which-key')

      wk.register({
        g = { ':Git<space>', '[g]it' },
      }, { prefix = '<leader>', silent = false })
    end
}
