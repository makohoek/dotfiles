"-------------------------------------------------------------------------------
" kir0gawa's .vimrc
" Feel free to copycat
" What you need :)
"-------------------------------------------------------------------------------

" Plugin loading with pathogen
"-------------------------------------------------------------------------------
execute pathogen#infect()

" Filetype
"-------------------------------------------------------------------------------
set nocompatible           "disable vi compatibility for better filetype
filetype plugin on         "Allows vim to detect filetype
filetype plugin indent on  "Allow specific plugins based on filetype


" Colorscheme and appearance
"-------------------------------------------------------------------------------
set t_Co=256;
syntax enable             "syntax highlighting on based on filetype
set cursorline        "show current line
set background=dark   "dark version of
colorscheme solarized "modified solarized
let g:xml_syntax_folding=1 "allow folding for xmls
set fdm=syntax        "folding method based on syntax
set showmatch         "show matching bracket
set number            "show line numbers


" indentation /tabs
"-------------------------------------------------------------------------------
set expandtab     "spaces instead of tabs
set shiftwidth=4  "number of spaces for each step of indent
set softtabstop=4 "number of spaces that a tab counts for
set autoindent    "Copy indent from current line when starting a new line
set backspace=indent,eol,start "backspace over autoindent, linebreaks and insert

"set 4 tabs when editing python
autocmd FileType python set sw=4 sts=4 et


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

" Show hidden tabs or trailing spaces
set listchars=tab:>~,nbsp:_,trail:~
set list

" See :help fo-table
set formatoptions+=r "multi comment when in insert mode
set formatoptions+=q "allows formatting of comments
set formatoptions+=c "allows automatic formatting of comments


" Go back to laster cursor position for each opened file
"-------------------------------------------------------------------------------
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif


" Shows column limit based on coding styles (80 chars)
"-------------------------------------------------------------------------------
let g:show_column_limit='off'
function! ToggleShowColumnLimit()
  if g:show_column_limit == 'off'
    set colorcolumn=80,100
    let g:show_column_limit='on'
  else
    set colorcolumn=0
    let g:show_column_limit='off'
  endif
endfunction

" Toggle background color easily
"-------------------------------------------------------------------------------
function! ToggleBackgroundColor()
  if &background == 'dark'
      set background=light
  else
      set background=dark
  endif
endfunction

" Toggle mouse on/off
"-------------------------------------------------------------------------------
function! ToggleMouse()
  if &mouse == 'a'
      set mouse=
  else
      set mouse=a
  endif
endfunction

" Autocomplete for some symbols
"-------------------------------------------------------------------------------
"comma always followed by a space
inoremap  ,  ,<Space>
""left brace/bracket always followed by right one
inoremap { {}<Left>

" Keybindings
"-------------------------------------------------------------------------------
"stop search higlight when hitting return key
nnoremap <CR><CR> :nohlsearch<CR>

let mapleader=","

" Command-T keybindings
nnoremap <silent> <Leader>o :CommandT<CR>

" YouCompleteMe keybindings
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>

"Taghighlight keybing
nnoremap <leader>tt :UpdateTypesFile<CR>

" UltiSnip keybindings
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" show columns for max length rules
nnoremap <leader>v :call ToggleShowColumnLimit()<CR>

" toggle background color for less eye hurting
nnoremap <leader>b :call ToggleBackgroundColor()<CR>

" toggle mouse in terminal
nnoremap <leader>m :call ToggleMouse()<CR>

" YouCompleteMe settings
"-------------------------------------------------------------------------------
let g:ycm_enable_diagnostic_signs = 0 "disable ugly error bar
" close annoying preview window after completion
let g:ycm_autoclose_preview_window_after_completion = 1

" Store temporary files in a central spot
"------------------------------------------------------------------------------
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
