return {
  "kdheepak/lazygit.nvim",
  -- optional for floating window border decoration
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
    require('telescope').load_extension('lazygit')
  end,
  keys = {
    { "<leader>gg", "<cmd>LazyGit<cr>", desc = "lazy[g]it" },
  },
}
