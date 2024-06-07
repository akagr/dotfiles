return {
  'MagicDuck/grug-far.nvim',
  dependencies = {
    'folke/which-key.nvim',
  },
  config = function()
    require('grug-far').setup({})
    local wk = require('which-key')
    wk.register({
      s = { '<cmd>GrugFar<cr>', '[s]earch' }
    }, { prefix = '<leader>', silent = false })
  end
}
