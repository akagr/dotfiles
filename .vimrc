" Akash Agrawal akashagrawal.me akagr[dot]outlook[dot]com

set nocompatible

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" External Bundles
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
" https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
call plug#begin('~/.vim/plugged')

" Add plugins here
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'mhartington/nvim-typescript', { 'commit': '70e36b80113c2d84663b0f86885320022943dd51' }
Plug 'editorconfig/editorconfig-vim'
Plug 'leafgarland/typescript-vim'
Plug 'isRuslan/vim-es6'
Plug 'posva/vim-vue'
Plug 'heavenshell/vim-jsdoc'
Plug 'w0rp/ale'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'keith/swift.vim'
Plug 'godlygeek/tabular'
Plug 'shime/vim-livedown'
Plug 'mattn/emmet-vim'
Plug 'elixir-editors/vim-elixir'
Plug 'ap/vim-css-color'
Plug 'Raimondi/delimitMate'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'ryanoasis/vim-devicons'
Plug 'arcticicestudio/nord-vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" To enable airline fonts, clone https://github.com/powerline/fonts
" and run `./install.sh`.
Plug 'vim-airline/vim-airline'

call plug#end()

filetype plugin indent on
syntax enable

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Essentials
set encoding=UTF-8
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

" Don't open tyepscript preview automatically
set completeopt-=preview

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Variables
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let ruby_no_expensive = 1

command! Q :q
let mapleader = ","
let g:mapleader = ","

" Add powerline fonts
let g:airline_powerline_fonts = 1

" Ale Configuration
let g:ale_fixers = {
  \ 'typescript': ['prettier'],
  \ 'ruby': [],
  \ 'rust': ['rustfmt']
  \ }
let g:ale_linters = {
  \ 'javascript': ['eslint'],
  \ 'ruby': ['standardrb', 'rubocop'],
  \ 'typescript': ['tslint', 'tsserver'],
  \ 'rust': ['rls']
  \ }
let g:ale_fix_on_save = 1
let g:ale_javascript_prettier_use_local_config = 1
let g:ale_typescript_prettier_use_local_config = 1
" Ale's default options are only available on rust's nightly compiler.
" Remvoing those
let g:ale_rust_rustc_options = ''
let g:ale_rust_rls_toolchain = 'stable'

" Language server variables
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'rls']
    \ }

" Editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" Hit enter in the file browser to open the selected
" file with :vsplit to the right of the browser.
let g:netrw_browse_split = 4
let g:netrw_altv = 1

" Default to tree mode
let g:netrw_liststyle=3
let g:netrw_winsize=20

" NERDTree config
let g:NERDTreeWinSize=60

" Deoplete settings
let g:deoplete#enable_at_startup = 1

" Use smartcase.
let g:deoplete#sources#tss#javascript_support = 1
call deoplete#custom#var('enable_buffer_path', v:true)
call deoplete#custom#option({
\ 'smart_case': v:true,
\ })

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
" Commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Rg stands for ripgrep
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(),
  \   <bang>0)

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
nnoremap <c-p> :Files<cr>
nnoremap <c-b> :Buffers<cr>
nnoremap <c-f> :Rg<space>
" Tab for cycling auto suggest
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" Neosnippets plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
" Add jsdoc for function
nmap <silent> <C-l> <Plug>(jsdoc)
" Go to typescript definition
nnoremap <leader>td :TSDef<cr>
" Go to next error
nnoremap <leader>an :ALENext<cr>

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
