return {
  'stevearc/oil.nvim',
  opts = {
    columns = {
      "icon",
      "permissions",
      "size",
      "mtime",
    }
  },
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    { '<leader>d', '<cmd>Oil<cr>', desc = '[d]irectory'}
  }
}
