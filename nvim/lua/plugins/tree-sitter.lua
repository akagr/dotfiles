return {
  'nvim-treesitter/nvim-treesitter',
  config = function ()
    require('nvim-treesitter.configs').setup({
      auto_install = true,
      highlight = {
        enable = true,
      },
      modules = {},
      sync_install = false,
      ensure_installed = {},
      ignore_install = {},
    })
  end
}
