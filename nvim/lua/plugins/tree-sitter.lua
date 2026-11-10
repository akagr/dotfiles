return {
  'nvim-treesitter/nvim-treesitter',
  branch = 'main',
  lazy = false,
  build = ':TSUpdate',
  config = function()
    local ts = require 'nvim-treesitter'
    local available = ts.get_available()

    -- Start highlighting on FileType, installing the parser first if needed.
    vim.api.nvim_create_autocmd('FileType', {
      group = vim.api.nvim_create_augroup('TreesitterStart', {}),
      callback = function(ev)
        local lang = vim.treesitter.language.get_lang(ev.match)
        if not lang or not vim.tbl_contains(available, lang) then
          return
        end
        if vim.tbl_contains(ts.get_installed 'parsers', lang) then
          vim.treesitter.start(ev.buf, lang)
          return
        end
        ts.install(lang):await(function(err)
          vim.schedule(function()
            if not err and vim.api.nvim_buf_is_valid(ev.buf) then
              vim.treesitter.start(ev.buf, lang)
            end
          end)
        end)
      end,
    })
  end,
}
