return {
  'nvim-telescope/telescope.nvim',
  cmd = 'Telescope',
  dependencies = {
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
    },
    'nvim-lua/plenary.nvim',
  },
  config = function()
    require('telescope').setup {}
    require('telescope').load_extension 'fzf'
  end,
}
