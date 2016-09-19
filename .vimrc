" Akash Agrawal akashagrawal.me akagr[dot]outlook[dot]com

set nocompatible

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" External Bundles
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
" https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off

call plug#begin('~/.vim/plugged')

Plug 'benekastah/neomake'
Plug 'neovimhaskell/haskell-vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'godlygeek/tabular'
Plug 'heavenshell/vim-jsdoc'
Plug 'kien/ctrlp.vim'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'Raimondi/delimitMate'
Plug 'scrooloose/nerdtree'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

call plug#end()
filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Essentials
set encoding=utf-8
set regexpengine=1
set clipboard=unnamed
set autoread nobackup nowritebackup noswapfile hidden
set laststatus=2
set ttyfast
set backspace=2
set wildmenu wildmode=longest:full,full
set ruler nowrap relativenumber number
set mouse=""

" Interface
set background=dark
colorscheme solarized
syntax on
set statusline=%F\ %m\ %{fugitive#statusline()}\ %y%=%l,%c\ %P
set statusline+=%#warningmsg#
set statusline+=\ %#ErrorMsg#%{neomake#statusline#LoclistStatus()}
set statusline+=%*

" Indentation
set expandtab tabstop=4 shiftwidth=4
set smartindent autoindent copyindent

" Search
set ignorecase smartcase
set hlsearch incsearch showmatch

" Regex
set magic

" Show trailing whitespaces
set list!
set listchars=trail:.,tab:--

" Add dash(-) to list of keywords. Avoids using it as word-separator
set iskeyword+=-
set iskeyword+=$
set iskeyword+="."

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Variables
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let ruby_no_expensive = 1

command! Q :q
let mapleader = ","
let g:mapleader = ","

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_user_command = {
  \ 'types': {
  \ 1: ['.git', 'cd %s && git ls-files | sort -r']
  \ },
  \ 'fallback': 'find %s -type f'
  \ }

" Hit enter in the file browser to open the selected
" file with :vsplit to the right of the browser.
let g:netrw_browse_split = 4
let g:netrw_altv = 1

" Default to tree mode
let g:netrw_liststyle=3
let g:netrw_winsize=20

" Deoplete settings
let g:deoplete#enable_at_startup = 1

" Use smartcase.
let g:deoplete#enable_smart_case = 1
let g:deoplete#auto_complete_start_length = 2
let g:deoplete#sources#tss#javascript_support = 1

" Add the snippets directory
let g:neosnippet#snippets_directory='~/.vim/snippets'
let g:neosnippet#disable_runtime_snippets = {
            \ '_' : 1,
            \}

" Expand carriage return on methods
let delimitMate_expand_cr=1

" Force specific linters for files
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_typescript_enabled_makers = ['tsc', 'tslint']

let s:enabled_options = [
    \ 'target', 'emitDecoratorMetadata', 'experimentalDecorators', 'module',
    \ 'noImplicitAny', 'rootDir', 'noEmit', 'allowSyntheticDefaultImports',
    \ 'noImplicitReturn', 'allowUnreachableCode', 'allowUnusedLabels'
    \ ]

function! neomake#makers#ft#typescript#tsc()
    let l:tsconfig = findfile('tsconfig.json', '.;')
    if len(l:tsconfig)
        let true = 1
        let false = 0
        let null = 0
        " ugly shortcut
        let l:jsonText = join(readfile(l:tsconfig, 'b'), '')
        let l:json = eval(l:jsonText)
        let l:option = get(l:json, 'compilerOptions', {})
        let l:option['noEmit'] = 1
        let l:args = []
        if !len(get(l:option, 'rootDir', ''))
            let l:option['rootDir'] = fnamemodify(l:tsconfig, ':h')
        endif
        for [key, value] in items(l:option)
            if index(s:enabled_options, key) == -1
                continue
            endif
            if value == 1
                call insert(l:args, '--'.key)
            elseif type(value) == type('')
                call insert(l:args, value)
                call insert(l:args, '--'.key)
            endif
        endfor
    else
        let l:args = [
            \ '-m', 'commonjs', '--noEmit', '--rootDir', '.'
        \ ]
    endif

    return {
        \ 'args': l:args,
        \ 'errorformat':
            \ '%E%f %#(%l\,%c): error %m,' .
            \ '%E%f %#(%l\,%c): %m,' .
            \ '%Eerror %m,' .
            \ '%C%\s%\+%m'
        \ }
endfunction

" Allow jsdoc for arrow notation
let g:jsdoc_allow_shorthand=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key Bindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle file explorer
noremap <silent> <C-E> :NERDTreeToggle<CR>
noremap <silent> <leader>n :NERDTreeFind<CR>
" Replaces escape in insert mode
inoremap jj <ESC>l
tnoremap jj <C-\><C-n>
" Search
noremap <space> /
" Travel one screen line at a time instead of logical line
noremap j gj
noremap k gk
" Save file
nnoremap <leader>s :w<cr>
" Un-highlight search results
noremap <silent> <leader>, :noh<cr>
" Insert empty line between braces on return
imap <expr> <CR> pumvisible() ? deoplete#close_popup() : '<Plug>delimitMateCR'
nnoremap <c-b> :CtrlPBuffer<cr>
" Tab for cycling auto suggest
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" Neosnippets plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
" Add jsdoc for function
nmap <silent> <C-l> <Plug>(jsdoc)

"""" Leader shortcuts
" Copy whole buffer
nnoremap <leader>a mzggyG`z
" Close buffer without closing window
nnoremap <leader>w :bp\|bd #<cr>
" Close buffer and window
nnoremap <leader>c :bd<cr>
" Quit window
nnoremap <leader>q :q<cr>
" Reload .vimrc
nnoremap <leader>r :source ~/.config/nvim/init.vim<cr>
" Toggle text wrapping
nnoremap <leader><space> :set wrap!<cr>
" Tabularize on =
nnoremap <leader>t= :Tabularize /=<cr>
vnoremap <leader>t= :Tabularize /=<cr>
" Tabularize on :
nnoremap <leader>t: :Tabularize /:<cr>
vnoremap <leader>t: :Tabularize /:<cr>
" Toggle folds
nnoremap <leader>f za

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au Filetype help nnoremap <CR> <C-]>
autocmd! BufWritePost * Neomake
