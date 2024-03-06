return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",
    "nvim-telescope/telescope.nvim",
    "folke/which-key.nvim",
  },
  config = function()
    require('neogit').setup({
      disable_hint = true,
      graph_style = "unicode",
      signs = {
        hunk = { "⏵", "⏷" },
        item = { "⏵", "⏷" },
        section = { "⏵", "⏷" },
      },
      integrations = {
        telescope = true,
        diffview = true,
      },
      console_timeout = 15000,
      mappings = {
        popup = {
          ["p"] = "PushPopup",
          ["F"] = "PullPopup",
          ["P"] = false
        }
      }
    })
    local wk = require('which-key')
    wk.register({
      g = {
        name = "+git",
        s = { '<cmd>Neogit<cr>', '[s]tatus' },
      }
    }, { prefix = '<leader>', silent = false })
  end,
}
