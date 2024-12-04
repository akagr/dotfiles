return {
  'folke/which-key.nvim',
  dependencies = {
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x' }
  },
  event = "VeryLazy",
  opts = {},
  config = function()
    local wk = require('which-key')

    wk.add({
      { "<leader>b", group = "buffer", silent = false },
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "list [b]uffers", silent = false },
      { "<leader>bd", ":bp<bar>sp<bar>bn<bar>confirm bd<cr>", desc = "[d]elete buffer", silent = false },
      { "<leader>bp", "<cmd>Telescope buffers only_cwd=true<cr>", desc = "list [p]roject buffers", silent = false },

      { "<leader>f", group = "file", silent = false },
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "[f]ind file", silent = false },
      { "<leader>fn", "<cmd>enew<cr>", desc = "[n]ew file", silent = false },
      { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "open [r]ecent file", silent = false },

      { "<leader>t", group = "telescope", silent = false },
      { "<leader>tt", "<cmd>Telescope resume<cr>", desc = "resume [t]elescope", silent = false },

      { "<leader>w", "<C-w>", desc = "window", remap = false, silent = false },
    })
  end
}
