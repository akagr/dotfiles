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
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'L3MON4D3/LuaSnip',
    'folke/which-key.nvim',
    'folke/trouble.nvim',
    'nvimtools/none-ls.nvim'
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

    -- lsp keybindings --
    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('UserLspConfig', {}),
      callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        local wk = require('which-key')
        wk.register({
          c = {
            name = "+code",
            d = { vim.lsp.buf.declaration, '[d]eclaration'},
            D = { vim.lsp.buf.definition, '[D]efinition'},
            h = { vim.lsp.buf.hover, '[h]elp'},
            i = { vim.lsp.buf.implementation, '[i]mplementation'},
            s = { vim.lsp.buf.signature_help, '[s]ignature'},
            T = { vim.lsp.buf.type_definition, '[T]ype definition'},
            t = { require('trouble').toggle, '[t]rouble'},
            n = { vim.lsp.buf.rename, 're[n]ame'},
            r = { vim.lsp.buf.references, '[r]eferences'},
            f = { function() vim.lsp.buf.format({async = true}) end, '[r]eferences'},
          },
        }, { prefix = '<leader>', silent = false, buffer = ev.buf })

        wk.register({
          ["ca"] = { vim.lsp.buf.code_action, '[a]ction'}
        }, { prefix = '<leader>', silent = false, buffer = ev.buf, mode = {'n', 'v'} })
      end,
    })

    -- mason --
    require('mason').setup()
    require('mason-lspconfig').setup({
      ensure_installed = {},
      handlers = {
        lsp_zero.default_setup,
      },
    })

    require'lspconfig'.yamlls.setup({
      -- on_attach = require'lsp'.common_on_attach,
      settings = {
        yaml = {
          format = {
            enable = true,
          },
          hover = true,
          completion = true,

          customTags = {
            "!fn", "!And", "!If", "!Not", "!Equals", "!Or", "!FindInMap sequence",
            "!Base64", "!Cidr", "!Ref", "!Ref Scalar", "!Sub", "!Sub sequence", "!GetAtt",
            "!GetAZs", "!ImportValue", "!Select", "!Split", "!Join sequence",
            "!Select sequence", "!Split sequence"
          },
        },
      },
    })

    -- none-ls --
    local null_ls = require("null-ls")
    null_ls.setup({
      sources = {
        null_ls.builtins.diagnostics.cfn_lint,
      }
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
        completeopt = 'menu,menuone,longest,noselect,popup'
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

    -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = 'buffer' }
      }
    })

    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = 'path' }
      }, {
        { name = 'cmdline' }
      })
    })
  end,
}
