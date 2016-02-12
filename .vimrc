"-------------------------------------------------------------------------------
" Makohoek's .vimrc
" Feel free to copycat
" What you need :)
"------------------------------------------------------------------------------
" {{{1 Plugin loading with vim-plug
"-------------------------------------------------------------------------------
set modelines=0
let g:plug_url_format = 'https://github.com/%s.git'
call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-tbone'
Plug 'tommcdo/vim-exchange'
Plug 'cbracken/vala.vim'
Plug 'mileszs/ack.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets' "default snippets for ultisnips
Plug 'jceb/vim-orgmode'
Plug 'vim-scripts/utl.vim' " for links in org-mode
Plug 'bogado/file-line'
Plug 'nanotech/jellybeans.vim'
Plug 'chriskempson/base16-vim'
Plug 'junegunn/seoul256.vim'
Plug 'Makohoek/pfw-vim-syntax'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
Plug 'vim-scripts/confluencewiki.vim'
Plug 'easymotion/vim-easymotion'
call plug#end()
set modelines=1

" load default vim man plugin
runtime ftplugin/man.vim

" {{{1 Filetype
"-------------------------------------------------------------------------------
set nocompatible           "disable vi compatibility for better filetype
filetype plugin on         "Allows vim to detect filetype
filetype plugin indent on  "Allow specific plugins based on filetype

" {{{1 Colorscheme and appearance
"-------------------------------------------------------------------------------
syntax enable         "syntax highlighting on based on filetype
set t_Co=256
set background=dark   "dark version of
colorscheme base16-tomorrow
set cursorline        "show current line
set fdm=syntax        "folding method based on syntax
set foldlevel=16      "open folds by default
set showmatch         "show matching bracket
set hlsearch          "show search highlighting
let g:xml_syntax_folding=1 "allow folding for xmls

" {{{1 Indentation spaces/tabs
"-------------------------------------------------------------------------------
set expandtab     "spaces instead of tabs
set shiftwidth=4  "number of spaces for each step of indent
set softtabstop=4 "number of spaces that a tab counts for
set autoindent    "Copy indent from current line when starting a new line
set backspace=indent,eol,start "backspace over autoindent, linebreaks and insert

"set 4 spaces when editing python
autocmd FileType python set sw=4 sts=4 ts=4 tabstop=4
autocmd FileType vim set sw=2 sts=2 ts=2 tabstop=2
autocmd FileType pfw set noet sw=4 sts=4 ts=4 tabstop=4
autocmd FileType ruby set sw=2 sts=2 ts=2 tabstop=2

" {{{1 status bar configuration
"-------------------------------------------------------------------------------
set ruler "show line and column number
set laststatus=2 "always show last status
set statusline=%<%f%h%w%m%r%=%y\ %l,%c\ %P "see :help statusline
set showcmd "show entered command

set wildmenu "command completion in ex mode
" wildignore options
set wildignore+=.DS_Store,.git/**,tmp/**,*.log,.bundle/**,node_modules/**,tags
set wildignore+=*.rbc,.rbx,*.scssc,*.sassc,.sass-cache,*.pyc,*.gem
set wildignore+=*.jpg,*.jpeg,*.tiff,*.gif,*.png,*.svg,*.psd,*.pdf

" {{{1 Search options
set smartcase "ignore case only when putting on a lowercase
set incsearch "start search when typing

" allow hidden buffers
set hidden

" Show hidden tabs or trailing spaces
set listchars=tab:▸\ ,nbsp:_,trail:¬
set list

" See :help fo-table
set formatoptions+=r "multi comment when in insert mode
set formatoptions+=q "allows formatting of comments
set formatoptions+=c "allows automatic formatting of comments

set ttimeout ttimeoutlen=0 notimeout " Disable timeout for Esc key
set ttyfast " Optimize for fast terminal connections
set lazyredraw " Don't redraw while executing macros (good performance config)

" {{{1 External programs
"-------------------------------------------------------------------------------
" use par for paragragh formatting
set formatprg=par\ -w80re

" Go back to laster cursor position for each opened file
"-------------------------------------------------------------------------------
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" {{{1 Functions

" {{{2 Shows column limit based on coding styles (80,100 chars)
"-------------------------------------------------------------------------------
function! ToggleShowColumnLimit()
  if &colorcolumn == '' || &colorcolumn == '0'
    set colorcolumn=80,100
  else
    set colorcolumn=0
  endif
endfunction

" {{{2 Remove trailing whitespaces (thanks vimcasts.org)
"-------------------------------------------------------------------------------
function! <SID>StripTrailingWhitespaces()
    " Preparation : save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" {{{2 Open the Url passed as argument (thanks tpope)
"-------------------------------------------------------------------------------
function! OpenURL(url)
    exe "silent !x-www-browser \"".a:url."\""
    redraw!
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)

" 2{{{ Call Uncrustify with a command
" Usage : :call Uncrustify('cpp')
"-------------------------------------------------------------------------------
" Restore cursor position, window position, and last search after running a
" command.
function! Preserve(command)
  " Save the last search.
  let search = @/

  " Save the current cursor position.
  let cursor_position = getpos('.')

  " Save the current window position.
  normal! H
  let window_position = getpos('.')
  call setpos('.', cursor_position)

  " Execute the command.
  execute a:command

  " Restore the last search.
  let @/ = search

  " Restore the previous window position.
  call setpos('.', window_position)
  normal! zt

  " Restore the previous cursor position.
  call setpos('.', cursor_position)
endfunction

" Specify path to your Uncrustify configuration file.
let g:uncrustify_cfg_file_path =
    \ shellescape(fnamemodify('~/dotfiles/my-uncrustify.cfg', ':p'))

" Don't forget to add Uncrustify executable to $PATH (on Unix) or
" %PATH% (on Windows) for this command to work.
function! Uncrustify(language)
  call Preserve(':silent %!uncrustify'
      \ . ' -q '
      \ . ' -l ' . a:language
      \ . ' -c ' . g:uncrustify_cfg_file_path)
endfunction

" 2{{{ FZF specific functions and commands
function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

command! FZFBuffers call fzf#run({
      \   'source':  reverse(<sid>buflist()),
      \   'sink':    function('<sid>bufopen'),
      \   'options': '+m',
      \   'down':    len(<sid>buflist()) + 2
      \ })

command! FZFMru call fzf#run({
    \ 'source': v:oldfiles,
    \ 'sink' : 'e ',
    \ 'options' : '-m',
    \ })

" set kernel style identation
function! SetCodingStyle(style)
  if a:style == 'kernel'
    set noet sw=8 sts=8 ts=8 tabstop=8
  else
    set et sw=4 sts=4 ts=4 tabstop=4
  endif
endfunction

" {{{1 Keybindings
"-------------------------------------------------------------------------------
let mapleader=" "
let maplocalleader=" "

"stop search higlight when hitting return key
nnoremap <leader>, :nohlsearch<CR>

" Insert a blank line below selected line
nnoremap <leader><CR> o<Esc>

" YouCompleteMe keybindings
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>

set spelllang=en_us "spell language which should be used

" Copy paste clipboard with tmux
vmap <Leader>y "+ygv:Tyank<CR>
nmap <Leader>p :Tput<CR>
vmap <Leader>p :Tput<CR>

" getting help in a fullscreen tab
map  <silent> <F1> :tabnew<CR>:h<CR>:on<CR>

" UltiSnip keybindings
let g:UltiSnipsExpandTrigger="<C-o>"
let g:UltiSnipsJumpForwardTrigger="<C-o>"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "customSnippets"]

" Bindings for the great tmux_navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
nnoremap <silent> <C-p> :TmuxNavigatePrevious<cr>

" show columns for max length rules
nnoremap <leader>v :call ToggleShowColumnLimit()<CR>

" FZF trough open files
nnoremap <silent> <Leader>o :FZF<CR>

" Navigate trough open buffers
nnoremap <silent> <Leader>b :FZFBuffers<CR>

" Navigate trough most recent used files
nnoremap <silent> <Leader>r :FZFMru<CR>

" Go to current file directory
nnoremap <leader>ff :cd %:h<CR>

" jump back and forth between files
nnoremap <leader><leader> <C-^>

" remove trailing whitespaces
nnoremap <leader>w :call <SID>StripTrailingWhitespaces()<CR>

" Navigate fast between all buffers
nnoremap <tab> :silent! bnext<CR>

" show element for syntax highlighting for finer tuning
" http://vim.wikia.com/wiki/Identify_the_syntax_highlighting_group_used_at_the_cursor
map <leader>cg :echo "hi<" . synIDattr(synID(line("."), col("."), 1), "name") . '> trans<'
            \ . synIDattr(synID(line("."), col("."), 0), "name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."), col("."), 1)), "name") . ">"<CR>

" C-a and C-e support for ex-mode
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" use enter to type :
nnoremap <enter> :

" search current word under cursor (found on tpopes vimrc)
nnoremap gs :OpenURL https://www.duckduckgo.com/search?q=<cword><CR>
" if we are doing cpp, use different search
autocmd FileType cpp nnoremap gs :OpenURL http://www.cplusplus.com/search.do?q=<cword><CR>

" commit message specific stuff
autocmd FileType gitcommit setlocal spell
autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1
autocmd FileType git,gitcommit exe "normal gg"

" Use autopep8 for formating python files with gq
autocmd FileType python setlocal formatprg=autopep8\ --aggressive\ --aggressive\ -

" {{{1 Plugin specific settings
"-------------------------------------------------------------------------------

" {{{2 YouCompleteMe settings
"-------------------------------------------------------------------------------
let g:ycm_enable_diagnostic_signs = 0 "disable ugly error bar
" close annoying preview window after completion
let g:ycm_autoclose_preview_window_after_completion = 1
" do NOT request config file
let g:ycm_confirm_extra_conf = 0

" {{{2 :Ack.vim settings
let g:ackprg = 'grep -rsni'

" {{{2 vim-cpp-enhanced highlight
let g:cpp_class_scope_highlight = 1

" {{{2 easymotion settings
let g:EasyMotion_use_upper = 1
map <Leader> <Plug>(easymotion-prefix)
let g:EasyMotion_keys = 'ASDGHKLQWERTYUIOPZXCVBNMFJ;'
hi link EasyMotionTargetDefault IncSearch

" {{{1 Store temporary files in a central spot
"------------------------------------------------------------------------------
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

if exists("+undofile")
  " Store undofile in to fixed location
  " undofile - This allows you to use undos after exiting and restarting
  " This, like swap and backups, uses .vim-undo first, then ~/.vim/undo
  " :help undo-persistence
  " This is only present in 7.3+
  set undofile
  set undodir=/var/tmp//,/tmp//,.
endif

if v:version > 703 || v:version == 703 && has("patch541")
  " Delete comment character when joining commented lines
  set formatoptions+=j
endif

" {{{1 Neovim specifics
"-------------------------------------------------------------------------------
if has('nvim')
  " quit terminal mode with Esc
  tnoremap <Esc> <Esc><C-\><C-n>
  " send escape to terminal
  tnoremap <C><Esc> <Esc>
endif

" {{{1 Local (specific) extra vimrc
"-------------------------------------------------------------------------------
let s:local_vimrc = expand('$HOME').'/.vimrc-extra'
if filereadable(s:local_vimrc)
  execute 'source' s:local_vimrc
endif

" {{{1 modeline
" vim: fdm=marker
