"""""""""""""""""""""
" kir0gawa's .vimrc " 
" plugins:          "
" - c.vim           "
" - vim-latex.vim   "
" - refactor.vim    "
"""""""""""""""""""""

" plugins with pathogen
execute pathogen#infect()

" set filetype plugin on for snipmate
filetype plugin on
filetype plugin indent on

"""""""""""""""
" colorscheme "
"""""""""""""""
syntax on
set cursorline
set background=dark
colorscheme solarized

" Go back to laster cursor position unless it is invalid or in an event
" handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

"""""""""""""""""""""
" indentation /tabs "
"""""""""""""""""""""
set expandtab
set shiftwidth=2
set softtabstop=2
set autoindent

set fdm=syntax

" show entered command
set showcmd

" show matching bracket
set showmatch
set backspace=indent,eol,start

" status bar configuration
set ruler
set laststatus=2
set statusline=%<%f%h%m%r%=%l,%c\ %P

" highlight searched items
set hlsearch
set ignorecase
set smartcase
set incsearch

" See Hidden tabs or space
set listchars=tab:>~,nbsp:_,trail:.
set list

" useful multi comment
set formatoptions+=r 

" line numbers
set number


"-------------------------------------------------------------------------------
"" comma always followed by a space
"-------------------------------------------------------------------------------
inoremap  ,  ,<Space>

"-------------------------------------------------------------------------------
"" autocomplete parenthesis, brackets and braces
"-------------------------------------------------------------------------------
inoremap ( ()<Left>
inoremap [ []<Left>
inoremap { {}<Left>

vnoremap ( s()<Esc>P<Right>%
vnoremap [ s[]<Esc>P<Right>%
vnoremap { s{}<Esc>P<Right>%

"-------------------------------------------------------------------------------
"" autocomplete quotes (visual and select mode)
"-------------------------------------------------------------------------------
xnoremap  '  s''<Esc>P<Right>
xnoremap  "  s""<Esc>P<Right>
xnoremap  `  s``<Esc>P<Right>


"""""""""""""""
" Keybindings "
"""""""""""""""
" Stop hlighting when hitting return key "
nnoremap <CR><CR> :nohlsearch<CR>

" Temporary files = NO "
set nobackup

"""""""""""""
" Command-T "
"""""""""""""
let mapleader=","

" For ycm "
let g:ycm_enable_diagnostic_signs = 0 "disable ugly error bar
" goto definition for C, Cpp
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
" close annoying preview window after completion
let g:ycm_autoclose_preview_window_after_completion = 1


" UltiSnip keybindings "
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
