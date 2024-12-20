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
        wk.add({

          { "<leader>c", group = "code", silent = false },
          { "<leader>ca", vim.lsp.buf.code_action, desc = "[a]ction", mode = { "n", "v" }, silent = false },
          { "<leader>cd", vim.lsp.buf.definition, desc = "[d]efinition", silent = false },
          { "<leader>cD", vim.lsp.buf.declaration, desc = "[D]eclaration", silent = false },
          { "<leader>cf", function() vim.lsp.buf.format({async = true}) end, desc = "[f]format", silent = false },
          { "<leader>ch", vim.lsp.buf.hover, desc = "[h]elp", silent = false },
          { "<leader>ci", vim.lsp.buf.implementation, desc = "[i]mplementation", silent = false },
          { "<leader>cn", vim.lsp.buf.rename, desc = "re[n]ame", silent = false },
          { "<leader>cr", vim.lsp.buf.references, desc = "[r]eferences", silent = false },
          { "<leader>cs", vim.lsp.buf.signature_help, desc = "[s]ignature", silent = false },
          { "<leader>ct", require('trouble').toggle, desc = "[t]rouble", silent = false },
          { "<leader>cT", vim.lsp.buf.type_definition, desc = "[T]ype definition", silent = false },
        })
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

    local lspconfig = require('lspconfig')

    lspconfig.yamlls.setup({
      -- on_attach = require'lsp'.common_on_attach,
      settings = {
        yaml = {
          format = {
            enable = true,
          },
          hover = false,
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

    lspconfig.helm_ls.setup {
      settings = {
        ['helm-ls'] = {
          yamlls = {
            path = "yaml-language-server",
          }
        }
      }
    }

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
        { name = 'buffer' },
        { name = 'path' },
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
