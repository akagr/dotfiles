local servers = { 'lua_ls', 'yamlls', 'helm_ls', 'gopls', 'pyright', 'ts_ls', 'eslint', 'jsonls', 'taplo', 'terraformls' }

-- Mason package names; servers above plus non-LSP tools.
local tools = {
  'lua-language-server',
  'yaml-language-server',
  'helm-ls',
  'gopls',
  'pyright',
  'typescript-language-server',
  'eslint-lsp',
  'json-lsp',
  'taplo',
  'terraform-ls',
  'cfn-lint',
  'shfmt',
  'stylua',
}

-- Mirrors none-ls' cfn_lint check so plain YAML/JSON isn't linted as CloudFormation.
local function is_cfn_template(bufnr)
  local found_resources = false
  for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
    if line:match '%s*"?AWSTemplateFormatVersion"?%s*:' then
      return true
    end
    found_resources = found_resources or line:match '"?Resources"?%s*:' ~= nil
    if found_resources and line:match '"?Type"?%s*:%s*"?\'?AWS::%w+::%w+"?\'?' then
      return true
    end
  end
  return false
end

return {
  {
    'mason-org/mason.nvim',
    cmd = 'Mason',
    opts = {},
  },
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = { 'mason-org/mason.nvim' },
    config = function()
      local registry = require 'mason-registry'
      registry.refresh(function()
        for _, name in ipairs(tools) do
          local ok, pkg = pcall(registry.get_package, name)
          if ok and not pkg:is_installed() then
            pkg:install()
          end
        end
      end)

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          require('which-key').add {
            buffer = ev.buf,
            { '<leader>c', group = 'code', silent = false },
            { '<leader>ca', vim.lsp.buf.code_action, desc = '[a]ction', mode = { 'n', 'v' }, silent = false },
            { '<leader>cd', vim.lsp.buf.definition, desc = '[d]efinition', silent = false },
            { '<leader>cD', vim.lsp.buf.declaration, desc = '[D]eclaration', silent = false },
            {
              '<leader>cf',
              function()
                vim.lsp.buf.format { async = true }
              end,
              desc = '[f]ormat',
              silent = false,
            },
            { '<leader>ch', vim.lsp.buf.hover, desc = '[h]elp', silent = false },
            { '<leader>ci', vim.lsp.buf.implementation, desc = '[i]mplementation', silent = false },
            { '<leader>cn', vim.lsp.buf.rename, desc = 're[n]ame', silent = false },
            { '<leader>cr', vim.lsp.buf.references, desc = '[r]eferences', silent = false },
            { '<leader>cs', vim.lsp.buf.signature_help, desc = '[s]ignature', silent = false },
            { '<leader>ct', vim.diagnostic.setqflist, desc = 'diagnos[t]ics', silent = false },
            { '<leader>cT', vim.lsp.buf.type_definition, desc = '[T]ype definition', silent = false },
          }
        end,
      })

      vim.lsp.config('yamlls', {
        settings = {
          yaml = {
            format = {
              enable = true,
            },
            hover = false,
            completion = true,

            customTags = {
              '!fn',
              '!And',
              '!If',
              '!Not',
              '!Equals',
              '!Or',
              '!FindInMap sequence',
              '!Base64',
              '!Cidr',
              '!Ref',
              '!Ref Scalar',
              '!Sub',
              '!Sub sequence',
              '!GetAtt',
              '!GetAZs',
              '!ImportValue',
              '!Select',
              '!Split',
              '!Join sequence',
              '!Select sequence',
              '!Split sequence',
            },
          },
        },
      })

      vim.lsp.config('helm_ls', {
        settings = {
          ['helm-ls'] = {
            yamlls = {
              path = 'yaml-language-server',
            },
          },
        },
      })

      vim.lsp.enable(servers)
    end,
  },
  {
    'saghen/blink.cmp',
    version = '1.*',
    opts = {
      keymap = {
        preset = 'none',
        ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
        ['<C-e>'] = { 'hide', 'fallback' },
        ['<CR>'] = { 'accept', 'fallback' },
        ['<Tab>'] = { 'select_next', 'fallback' },
        ['<S-Tab>'] = { 'select_prev', 'fallback' },
        ['<C-j>'] = { 'scroll_documentation_down', 'fallback' },
        ['<C-k>'] = { 'scroll_documentation_up', 'fallback' },
        ['<C-f>'] = { 'snippet_forward', 'fallback' },
        ['<C-b>'] = { 'snippet_backward', 'fallback' },
      },
      completion = {
        menu = { border = 'rounded' },
        documentation = { auto_show = true, window = { border = 'rounded' } },
      },
      sources = {
        default = { 'lazydev', 'lsp', 'path', 'snippets', 'buffer' },
        providers = {
          lazydev = { name = 'LazyDev', module = 'lazydev.integrations.blink', score_offset = 100 },
        },
      },
    },
  },
  {
    'folke/lazydev.nvim',
    ft = 'lua',
    opts = {
      library = {
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
      },
    },
  },
  {
    'mfussenegger/nvim-lint',
    event = { 'BufReadPost', 'BufNewFile' },
    config = function()
      local lint = require 'lint'
      lint.linters_by_ft = {
        yaml = { 'cfn_lint' },
        json = { 'cfn_lint' },
      }
      vim.api.nvim_create_autocmd({ 'BufReadPost', 'BufWritePost', 'InsertLeave' }, {
        group = vim.api.nvim_create_augroup('UserLint', {}),
        callback = function(ev)
          if lint.linters_by_ft[vim.bo[ev.buf].filetype] and is_cfn_template(ev.buf) then
            lint.try_lint()
          end
        end,
      })
    end,
  },
}
