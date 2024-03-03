-- autoformat.lua
--
-- Use your language server to automatically format your code on save.
-- Adds additional commands as well to manage the behavior

return {
  'VonHeikemen/lsp-zero.nvim',
  branch = 'v3.x',
  dependencies = {
    'neovim/nvim-lspconfig',
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-nvim-lsp',
    'L3MON4D3/LuaSnip',
  },
  event = { 'BufReadPre', 'BufNewFile' },
  cmd = 'Mason',
  config = function()
    -- lsp zero --
    local lsp_zero = require('lsp-zero')
    lsp_zero.on_attach(function(_, bufnr)
      -- see :help lsp-zero-keybindings
      -- to learn the available actions
      lsp_zero.default_keymaps({buffer = bufnr})
    end)

    -- mason --
    require('mason').setup()
    require('mason-lspconfig').setup({
      ensure_installed = {},
      handlers = {
        lsp_zero.default_setup,
      },
    })

    -- nvim-cmp --
    local cmp = require('cmp')
    local cmp_action = require('lsp-zero').cmp_action()
    cmp.setup({
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
      }, {
        { name = 'buffer' }
      }),
      mapping = {
          ['<CR>'] = cmp.mapping.confirm({ select = true }),
          ['<Tab>'] = cmp.mapping.select_next_item(),
          ['<S-Tab>'] = cmp.mapping.select_prev_item(),
          ['<C-j>'] = cmp.mapping.scroll_docs(1),
          ['<C-k>'] = cmp.mapping.scroll_docs(-1),
          ['<C-f>'] = cmp_action.luasnip_jump_forward(),
          ['<C-b>'] = cmp_action.luasnip_jump_backward(),
      },
      completion = {
        completeopt = 'menu,menuone,longest'
      },
      window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
      },
      snippet = {
        expand = function(args)
          require('luasnip').lsp_expand(args.body)
        end
      }
    })
  end,
}
