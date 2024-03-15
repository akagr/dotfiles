return {
  'nvim-pack/nvim-spectre',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'folke/which-key.nvim',
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x' },
  },
  config = function()
    local wk = require('which-key')
    local specter = require('spectre')
    wk.register({
      s = {
        name = "+search",
        s = { function () specter.open_visual({select_word = true}) end, '[s]earch' },
        f = { function () specter.open_file_search({select_word = true}) end, '[f]ile' },
        t = { '<cmd>Telescope live_grep<cr>', '[t]elescope'}
      },
    }, { prefix = '<leader>', silent = false })
  end
}
