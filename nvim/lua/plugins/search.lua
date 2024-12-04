return {
  'MagicDuck/grug-far.nvim',
  dependencies = {
    'folke/which-key.nvim',
  },
  config = function()
    require('grug-far').setup({})
    local wk = require('which-key')
    wk.add({
      { "<leader>s", "<cmd>GrugFar<cr>", desc = "[s]earch", silent = false },
    })
  end
}
