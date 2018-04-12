" Akash Agrawal akashagrawal.me akagr[dot]outlook[dot]com

set nocompatible

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" External Bundles
" curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
" ./installer.sh ~/.vim/dein
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.vim/dein')
  call dein#begin('~/.vim/dein')

  " Required
  call dein#add('~/.vim/dein/repos/github.com/Shougo/dein.vim')

  " Add plugins here
  call dein#add('Shougo/vimproc.vim', { 'build': 'make' })
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('mhartington/nvim-typescript')
  call dein#add('editorconfig/editorconfig-vim')
  call dein#add('leafgarland/typescript-vim')
  call dein#add('heavenshell/vim-jsdoc')
  call dein#add('w0rp/ale')
  call dein#add('neovimhaskell/haskell-vim')
  call dein#add('godlygeek/tabular')
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('mattn/emmet-vim')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('Raimondi/delimitMate')
  call dein#add('scrooloose/nerdtree')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-surround')
  call dein#add('arcticicestudio/nord-vim')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

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
set ruler nowrap number
set mouse=""

" Interface
set background=dark
colorscheme nord
set statusline=%F\ %m\ %{fugitive#statusline()}\ %y%=%l,%c\ %P
set statusline+=%#warningmsg#
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

" Ale Configuration
let g:ale_fixers = {
  \ 'typescript': ['prettier'],
  \ }
let g:ale_linters = {
  \ 'javascript': ['eslint'],
  \ 'typescript': ['tslint', 'tsserver'],
  \ }
let g:ale_fix_on_save = 1
let g:ale_javascript_prettier_use_local_config = 1
let g:ale_typescript_prettier_use_local_config = 1

" Editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
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
let g:deoplete#file#enable_buffer_path = 1

" Add the snippets directory
let g:neosnippet#snippets_directory='~/.vim/snippets'
let g:neosnippet#disable_runtime_snippets = {
            \ '_' : 1,
            \}

" Expand carriage return on methods
let delimitMate_expand_cr=1

let s:enabled_options = [
    \ 'target', 'emitDecoratorMetadata', 'experimentalDecorators', 'module',
    \ 'noImplicitAny', 'rootDir', 'noEmit', 'allowSyntheticDefaultImports',
    \ 'noImplicitReturn', 'allowUnreachableCode', 'allowUnusedLabels'
    \ ]

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
" Open git status
nnoremap <leader>gs :Gstatus<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Autocommands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Open links in help file using enter key
au Filetype help nnoremap <CR> <C-]>
