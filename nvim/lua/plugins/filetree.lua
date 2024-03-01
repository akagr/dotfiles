return {
  'nvim-tree/nvim-tree.lua',
  version = '*',
  lazy = false,
  dependencies = {
    'nvim-tree/nvim-web-devicons',
    'nvim-lua/plenary.nvim'
  },
  config = function()
    require('nvim-tree').setup({
      update_focused_file = {
        enable = true,
        update_root = true
      }
    })
  end,
  keys = {
    { '<leader>n', '<cmd>NvimTreeToggle<cr>', desc = '[n]vim tree' }
  }
}
