-- Akash Agrawal <akagr@outlook.com>
-- Inspired (read: stolen) from https://github.com/nvim-lua/kickstart.nvim

-- Set <comma> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ','
vim.g.maplocalleader = ','

-- Install package manager
-- https://github.com/folke/lazy.nvim
-- `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable',
        lazypath
    }
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
    -- Detects tabstops and shiftwidth automatically
    'tpope/vim-sleuth',

    -- Shows possible key combinations after pressing a key
    { 'folke/which-key.nvim', opts = {} },

    -- Add documentation for vim's lua api
    'folke/neodev.nvim',

    -- "gc" to comment visual regions/lines
    { 'numToStr/Comment.nvim', opts = {} },

    { import = 'plugins' },
}, {})

-- General vim options

-- Set highlight on search
vim.o.hlsearch = false

-- Turn on line numbers
vim.wo.number = true

-- Sync clipboard between OS and Neovim.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Need xterm-256color terminal to support this
vim.o.termguicolors = true
