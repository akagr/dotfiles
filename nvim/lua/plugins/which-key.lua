-- Open lazygit in a new tab at the current file's git root; close the tab when it exits.
local function lazygit()
  local root = vim.fs.root(0, '.git') or vim.uv.cwd()
  vim.cmd.tabnew()
  local buf = vim.api.nvim_get_current_buf()
  vim.fn.jobstart('lazygit', {
    term = true,
    cwd = root,
    on_exit = function()
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(buf) then
          vim.api.nvim_buf_delete(buf, { force = true })
        end
      end)
    end,
  })
  vim.cmd.startinsert()
end

return {
  'folke/which-key.nvim',
  event = 'VeryLazy',
  opts = {
    spec = {
      { '<leader>b', group = 'buffer', silent = false },
      { '<leader>bb', '<cmd>Telescope buffers<cr>', desc = 'list [b]uffers', silent = false },
      { '<leader>bd', ':bp<bar>sp<bar>bn<bar>confirm bd<cr>', desc = '[d]elete buffer', silent = false },
      { '<leader>bp', '<cmd>Telescope buffers only_cwd=true<cr>', desc = 'list [p]roject buffers', silent = false },

      { '<leader>f', group = 'file', silent = false },
      { '<leader>ff', '<cmd>Telescope find_files<cr>', desc = '[f]ind file', silent = false },
      { '<leader>fg', '<cmd>Telescope live_grep<cr>', desc = '[g]rep', silent = false },
      { '<leader>fn', '<cmd>enew<cr>', desc = '[n]ew file', silent = false },
      { '<leader>fr', '<cmd>Telescope oldfiles<cr>', desc = 'open [r]ecent file', silent = false },

      { '<leader>g', lazygit, desc = 'lazy[g]it' },

      { '<leader>t', group = 'telescope', silent = false },
      { '<leader>tt', '<cmd>Telescope resume<cr>', desc = 'resume [t]elescope', silent = false },

      { '<leader>w', '<C-w>', desc = 'window', remap = false, silent = false },
    },
  },
}
