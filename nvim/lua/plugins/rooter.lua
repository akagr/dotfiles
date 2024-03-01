-- Lua
return {
  "ahmedkhalf/project.nvim",
  lazy = false,
  dependencies = {
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x' },
  },
  config = function()
    require("project_nvim").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
    require('telescope').load_extension('projects')
  end,
  keys = {
    {
      "<leader>p",
      function()
        require 'telescope'.extensions.projects.projects {}
      end,
      desc = "[p]rojects",
    },
  },
}
