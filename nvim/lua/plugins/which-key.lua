return {
  'folke/which-key.nvim',
  dependencies = {
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x' }
  },
  event = "VeryLazy",
  opts = {},
  config = function()
    local wk = require('which-key')

    wk.register({
      f = {
        name = "+file",
        f = { "<cmd>Telescope find_files<cr>", "[f]ind file" },
        r = { "<cmd>Telescope oldfiles<cr>", "open [r]ecent file" },
        n = { "<cmd>enew<cr>", "[n]ew file" },
      },
      b = {
        name = "+buffer",
        b = { "<cmd>Telescope buffers<cr>", "list [b]uffers" },
        p = { "<cmd>Telescope buffers only_cwd=true<cr>", "list [p]roject buffers" },
        d = { ":bp<bar>sp<bar>bn<bar>bd<cr>", "[d]elete buffer" },
      },
      s = {
        name = "+search",
        s = { "<cmd>Telescope live_grep<cr>", "[s]earch cwd"}
      },
      t = {
        name = "+telescope",
        t = { "<cmd>Telescope resume<cr>", "resume [t]elescope" },
      },
    }, { prefix = '<leader>', silent = false })

    wk.register({
      w = { '<C-w>', '+window' },
    }, { prefix = '<leader>', noremap = false, silent = false })
  end
}
