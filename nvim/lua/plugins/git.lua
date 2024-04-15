return {
  'tpope/vim-fugitive',
  dependencies = {
    'folke/which-key.nvim'
  },
  config = function()
    local wk = require('which-key')

    wk.register({
      g = { ":Git ", "fu[git]ive"}
    }, { prefix = '<leader>', silent = false })
  end
}
