" Akash Agrawal akashagrawal.me akagr[dot]outlook[dot]com

set nocompatible

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" External Bundles
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
" https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off

call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-fugitive'
Plug 'kien/ctrlp.vim'
Plug 'godlygeek/tabular'
Plug 'mileszs/ack.vim'
Plug 'Shougo/neocomplcache.vim'
Plug 'Shougo/neosnippet'
Plug 'chilicuil/vim-sprunge'
Plug 'tpope/vim-surround'
Plug 'dag/vim2hs'
Plug 'scrooloose/nerdtree'
Plug 'mattn/emmet-vim'
Plug 'benekastah/neomake'
Plug 'heavenshell/vim-jsdoc'

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
set mouse=a
set wildmenu wildmode=longest:full,full
set ruler nowrap relativenumber number

" Interface
colorscheme base16-default
set background=dark
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

" Neocomplcache settings
let g:neocomplcache_enable_at_startup = 1

" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_auto_completion_start_length = 2

" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_same_filetype_lists = {}
let g:neocomplcache_same_filetype_lists._ = '_'

" Add the snippets directory
let g:neosnippet#snippets_directory='~/.vim/snippets'
let g:neosnippet#disable_runtime_snippets = {
            \ '_' : 1,
            \}

" Expand carriage return on methods
let delimitMate_expand_cr=1

" Force specific linters for files
let g:neomake_javascript_enabled_makers = ['eslint']

" Allow jsdoc for arrow notation
let g:jsdoc_allow_shorthand=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key Bindings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle file explorer
noremap <silent> <C-E> :NERDTreeToggle<CR>
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
imap <expr> <CR> pumvisible() ? neocomplcache#close_popup() : '<Plug>delimitMateCR'
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
nnoremap <leader>r :source ~/.nvimrc<cr>
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
