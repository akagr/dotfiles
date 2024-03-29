local permission_hlgroups = {
  ['-'] = 'NonText',
  ['r'] = 'DiagnosticSignOk',
  ['w'] = 'DiagnosticSignWarn',
  ['x'] = 'DiagnosticSignError',
}

return {
  'stevearc/oil.nvim',
  opts = {
    columns = {
      {
        "permissions",
        highlight = function(permission_str)
          local hls = {}
          for i = 1, #permission_str do
            local char = permission_str:sub(i, i)
            table.insert(hls, { permission_hlgroups[char], i - 1, i })
          end
          return hls
        end,
      },
      { 'size', highlight = 'Special' },
      { 'mtime', highlight = 'Number' },
      { "icon", add_padding = false },
    },
    win_options = {
      winbar = "%{v:lua.require('oil').get_current_dir()}",
    },
    keymaps = {
      ['<'] = 'actions.parent',
      ['>'] = 'actions.select',
    },
  },
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    { '<leader>d', '<cmd>Oil<cr>', desc = '[d]irectory'}
  }
}
