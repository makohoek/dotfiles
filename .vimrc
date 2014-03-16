"-------------------------------------------------------------------------------
" kir0gawa's .vimrc
" Feel free to copycat
" What you need :)
"-------------------------------------------------------------------------------

" Plugin loading with pathogen
"-------------------------------------------------------------------------------
execute pathogen#infect()

set nocompatible           "disable vi compatibility for better filetype
filetype plugin on         "Allows vim to detect filetype
filetype plugin indent on  "Allow specific plugins based on filetype


" Colorscheme and appearance
"-------------------------------------------------------------------------------
syntax on             "syntax highlighting on based on filetype
set cursorline        "show current line
set background=dark   "dark version of
colorscheme solarized "ethan schoovers amazing solarized
set fdm=syntax        "folding method based on syntax
set showmatch         "show matching bracket
set number            "show line numbers

" indentation /tabs
"-------------------------------------------------------------------------------
set expandtab     "spaces instead of tabs
set shiftwidth=2  "number of spaces for each step of indent
set softtabstop=2 "number of spaces that a tab counts for
set autoindent    "Copy indent from current line when starting a new line
set backspace=indent,eol,start "backspace over autoindent, linebreaks and insert

" status bar configuration
"-------------------------------------------------------------------------------
set ruler "show line and column number
set laststatus=2 "always show last status
set statusline=%<%f%h%w%m%r%=%y\ %l,%c\ %P "see :help statusline
set showcmd "show entered command

" Search options
"-------------------------------------------------------------------------------
set hlsearch "highlight searched items
set smartcase "ignore case only when putting on a lowercase
set incsearch "start search when typing

" See Hidden tabs or space
set listchars=tab:>~,nbsp:_,trail:.
set list

" See :help fo-table
set formatoptions+=r "multi comment when in insert mode
set formatoptions+=q "allows formatting of comments
set formatoptions+=c "allows automatic formatting of comments



" Go back to laster cursor position for earch opened file
"-------------------------------------------------------------------------------
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif


" comma always followed by a space
"-------------------------------------------------------------------------------
inoremap  ,  ,<Space>

" autocomplete parenthesis, brackets and braces
"-------------------------------------------------------------------------------
inoremap ( ()<Left>
inoremap [ []<Left>
inoremap { {}<Left>

vnoremap ( s()<Esc>P<Right>%
vnoremap [ s[]<Esc>P<Right>%
vnoremap { s{}<Esc>P<Right>%

" autocomplete quotes (visual and select mode)
"-------------------------------------------------------------------------------
xnoremap  '  s''<Esc>P<Right>
xnoremap  "  s""<Esc>P<Right>
xnoremap  `  s``<Esc>P<Right>


" Keybindings
"-------------------------------------------------------------------------------
" Stop hlighting when hitting return key "
nnoremap <CR><CR> :nohlsearch<CR>

" Temporary files = NO "
set nobackup

" Command-T
"-------------------------------------------------------------------------------
let mapleader=","

" For ycm "
let g:ycm_enable_diagnostic_signs = 0 "disable ugly error bar
" goto definition for C, Cpp
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
" close annoying preview window after completion
let g:ycm_autoclose_preview_window_after_completion = 1


" UltiSnip keybindings "
"-------------------------------------------------------------------------------
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" show columns for max length rules
" TODO: change this for a toggleMethod
nnoremap <leader>v :set colorcolumn=80<CR>
