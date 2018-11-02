"-------------------------------------------------------------------------------
" makohoek's .vimrc
" Feel free to copycat
" What you need :)
"------------------------------------------------------------------------------
" {{{1 Plugin loading with vim-plug
"-------------------------------------------------------------------------------
if filereadable(expand("~/.vimrc.plugins"))
  source ~/.vimrc.plugins
endif

" Load additional themes which are not in standard plug directory
set runtimepath+=~/.vim/plugged/tomorrow-theme/vim

" {{{1 Filetype
"-------------------------------------------------------------------------------
set nocompatible           "disable vi compatibility for better filetype
filetype plugin on         "Allows vim to detect filetype
filetype plugin indent on  "Allow specific plugins based on filetype

" {{{1 Colorscheme and appearance
"-------------------------------------------------------------------------------
syntax enable         "syntax highlighting on based on filetype
set t_Co=256

set background=dark
colorscheme zenburn
set cursorline        "show current line
set fdm=syntax        "folding method based on syntax
set foldlevel=16      "open folds by default
set showmatch         "show matching bracket
set hlsearch          "show search highlighting
let g:xml_syntax_folding=1 "allow folding for xmls

" patch zenburn theme to have statusbar a bit brighter
" This helps me to see which window is active
hi StatusLine guifg=#313633 guibg=#ccdc90 ctermfg=239 ctermbg=186

" patch zenburn for terminal cursor in neovim
hi link TermCursor Cursor
" hi TermCursorNC  guibg=#484848                         ctermbg=238
hi TermCursorNC  guifg=#000d18 guibg=#8faf9f gui=bold       ctermfg=233 ctermbg=109 cterm=bold


" {{{1 Indentation spaces/tabs
"-------------------------------------------------------------------------------
set expandtab     "spaces instead of tabs
set shiftwidth=4  "number of spaces for each step of indent
set softtabstop=4 "number of spaces that a tab counts for
set autoindent    "Copy indent from current line when starting a new line
set backspace=indent,eol,start "backspace over autoindent, linebreaks and insert

" {{{1 status bar configuration
"-------------------------------------------------------------------------------
set ruler "show line and column number
set laststatus=2 "always show last status
set statusline=%<           "truncate at start
set statusline+=%f          "show filename/filepath
set statusline+=%h          "help flag
set statusline+=%w          "preview flag
set statusline+=%m          "modified flag
set statusline+=%r          "readonly flag"
set statusline+=%=          "next section
set statusline+=%y          "show filetype
set statusline+=\ %l,%c\    "lines and colums
set statusline+=%P          "scroll percentage

set showcmd "show entered command

set wildmenu "command completion in ex mode
" wildignore options
set wildignore+=.DS_Store,.git/**,tmp/**,*.log,.bundle/**,node_modules/**,tags
set wildignore+=*.rbc,.rbx,*.scssc,*.sassc,.sass-cache,*.pyc,*.gem
set wildignore+=*.jpg,*.jpeg,*.tiff,*.gif,*.png,*.svg,*.psd,*.pdf

set splitright

" {{{1 Search options
set smartcase "ignore case only when putting on a lowercase
set incsearch "start search when typing

set spelllang=en_us "spell language which should be used

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

" always use clipboard
set clipboard+=unnamedplus


" {{{1 External programs
"-------------------------------------------------------------------------------
" use par for paragragh formatting
set formatprg=par


" {{{1 Functions
"-------------------------------------------------------------------------------
" {{{2 Shows column limit based on coding styles (80,100 chars)
function! ToggleShowColumnLimit()
  if &colorcolumn == '' || &colorcolumn == '0'
    set colorcolumn=80,100
  else
    set colorcolumn=0
  endif
endfunction

" {{{2 Remove trailing whitespaces (thanks vimcasts.org)
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
function! OpenURL(url)
    exe "silent !open \"".a:url."\""
    redraw!
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)

" {{{2 FZF specific functions and commands
function! s:buflist()
  redir => ls
  silent ls
  redir END
  return split(ls, '\n')
endfunction

function! s:bufopen(e)
  execute 'buffer' matchstr(a:e, '^[ 0-9]*')
endfunction

" set kernel style identation
function! SetCodingStyle(style)
  if a:style == 'kernel'
    set noet sw=4 sts=4 ts=4 tabstop=4
  else
    set et sw=4 sts=4 ts=4 tabstop=4
  endif
endfunction

" buffer delete function but keep the splits
" borrowed from:
" http://stackoverflow.com/questions/1444322/how-can-i-close-a-buffer-without-closing-the-window
function! BufferDelete()
    if &modified
        echohl ErrorMsg
        echomsg "No write since last change. Not closing buffer."
        echohl NONE
    else
        let s:total_nr_buffers = len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))

        if s:total_nr_buffers == 1
            bdelete!
            echo "Buffer deleted. Created new buffer."
        else
            buffer #
            bdelete! #
            echo "Buffer deleted."
        endif
    endif
endfunction

function! BufferRename(name)
    if exists("b:term_title")
        " we are in a terminal buffer, we must prepend with term://
        let l:buffer_name = "term://" . a:name
    else
        let l:buffer_name = a:name
    endif
    execute 'file ' . l:buffer_name
endfunction

function! BufferRenameInteractive()
    let l:new_name = input("New buffer name: ")
    if l:new_name == ""
        echomsg "cannot give empty name"
    else
        call BufferRename(l:new_name)
    endif
endfunction

" copy currents buffer full filepath to the clipboard
" useful for copy-pasting to share code snippets
function! CopyFullFilepath()
    let l:filepath = expand("%:p")
    echom l:filepath
    let @* = l:filepath
endfunction


" {{{1 Project stuff (poor man's projectile)
"-------------------------------------------------------------------------------
" Project related things
let s:work_vimrc = expand('$HOME').'/work/.work.vimrc'
if filereadable(s:work_vimrc)
  execute 'source' s:work_vimrc
endif

if !exists('g:makohoek_projects')
  let g:makohoek_projects = [
        \ '~/code/cpp/hackerrank/',
        \ '~/code/gerrit_scripts/',
        \ '~/code/rust/neovim-cmd/',
        \ '~/code/docker/docker-dotfiles',
        \ ]
endif

" This is called each time we open a new project
function! ProjectEditBookmark(bookmark)
    execute "tabedit " . a:bookmark
    execute "tcd " . a:bookmark
endfunction

command! -bang ProjectBookmarks call fzf#run({
    \ 'source': g:makohoek_projects,
    \ 'sink' : function('ProjectEditBookmark'),
    \ 'options' : '-m',
    \ })


" {{{1 Keybindings
"-------------------------------------------------------------------------------
let mapleader=" "
let maplocalleader=" "

" navigate in wrapped lines easily
noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj
noremap <buffer> <silent> ^ g^
noremap <buffer> <silent> $ g$

" C-a and C-e support for ex-mode
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" don't do anything for ex mode
" I never use that feature
noremap Q <nop>

" {{{2 Leader based keybindings
" {{{3 Visual stuff
"stop search higlight when hitting return key
nnoremap <leader>, :nohlsearch<CR>
" show columns for max length rules
nnoremap <leader>v :call ToggleShowColumnLimit()<CR>

" Insert a blank line below selected line
nnoremap <leader><CR> o<Esc>

" {{{3 Help and commands
" FZF commmand source
nnoremap <leader><leader> :Commands<CR>
" Navigate through all help
nnoremap <silent> <Leader>hh :Helptags<CR>

" {{{3 Project
nnoremap <leader>pp :ProjectBookmarks<CR>

" {{{3 Files
" FZF trough open files
nnoremap <leader>ff :FZF<CR>
nnoremap <leader>fy :call CopyFullFilepath()<CR>
nnoremap <leader>fed :e ~/.vimrc<CR>
nnoremap <leader>feR :source ~/.vimrc<CR>

" {{{3 Buffers
" Navigate trough open buffers
nnoremap <silent> <Leader>bb :History<CR>
" delete current buffer, keep the split
nnoremap <leader>bd :call BufferDelete()<CR>
" rename current buffer
nnoremap <leader>br :call BufferRenameInteractive()<CR>
" home buffer
nnoremap <leader>bh :Startify<CR>
" new scratch buffer
nnoremap <leader>bs :enew<CR>

" {{{3 applications (like terminal and hangups)
nnoremap <leader>at :terminal<CR>
nnoremap <leader>ah :terminal hangups<CR>i

" {{{3 Git
" fugitive related
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :GFiles?<CR>
nnoremap <leader>gc :Gcommit<CR>
" this is FZF, browsing through all the commits
nnoremap <leader>gl :Commits<CR>

" {{{3 Grepper/searching
nnoremap <silent> <Leader>ss :Grepper -tool ag -cword<CR>
" search current word under cursor (found on tpopes vimrc)
nnoremap <silent> <Leader>sw :OpenURL https://www.duckduckgo.com/search?q=<cword><CR>

" {{{3 Window related
nnoremap <leader>wh <C-w>h
nnoremap <leader>wj <C-w>j
nnoremap <leader>wk <C-w>k
nnoremap <leader>wl <C-w>l
nnoremap <leader>ww <C-w>w
nnoremap <leader>w= <C-w>=
nnoremap <leader>wo <C-w>o


" {{{1 Abbreviations
"-------------------------------------------------------------------------------


" {{{1 Autocommands
"-------------------------------------------------------------------------------
" Go back to laster cursor position for each opened file
augroup makohoek_buffer_tweaks
  autocmd!
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
augroup END

augroup makohoek_python
  autocmd!
  "set 4 spaces when editing python
  autocmd FileType python set sw=4 sts=4 ts=4 tabstop=4
  " Use autopep8 for formating python files with gq
  autocmd FileType python setlocal formatprg=autopep8\ --aggressive\ --aggressive\ -
augroup END

augroup makohoek_vimfiles
  autocmd!
  autocmd FileType vim set sw=2 sts=2 ts=2 tabstop=2
augroup END

augroup makohoek_git
  autocmd!
  " commit message specific stuff
  autocmd FileType gitcommit setlocal spell
  autocmd FileType git,gitcommit setlocal foldmethod=syntax foldlevel=1
  autocmd FileType git,gitcommit setlocal colorcolumn=72
  autocmd FileType git,gitcommit setlocal textwidth=72
  autocmd BufReadPost */COMMIT_EDITMSG exe "normal gg"
  autocmd FileType git,gitcommit :iabbrev <buffer> tr Tracked-On:
augroup END

augroup makohoek_cpp
  autocmd!
  " if we are doing cpp, use different search
  autocmd FileType cpp nnoremap <silent> <Leader>sw :OpenURL http://en.cppreference.com/mwiki/index.php?title=Special%3ASearch&search=<cword><CR>
augroup END


" {{{1 Plugin specific settings
"-------------------------------------------------------------------------------
" {{{2 vim-cpp-enhanced highlight
let g:cpp_class_scope_highlight = 1

" {{{2 Grepper settings
runtime plugin/grepper.vim    " initialize g:grepper with default values
let g:grepper.dir = 'repo,cwd'

" {{{2 Startify settings
let g:startify_custom_header = ['']

" {{{2 vim-orgmode
let g:org_plugins = ['Hyperlinks']

" {{{2 vim-pandoc and vim-pandoc-syntax
" don't touch my bindings
let g:pandoc#keyboard#use_default_mappings = 0
" Some modules I don't need
let g:pandoc#modules#disabled = ["folding", "templates", "bibliography", "yaml"]
" I think this looks ugly
let g:pandoc#syntax#conceal#use = 0
" support python and bash syntax
let g:pandoc#syntax#codeblocks#embeds#langs = ["python", "bash=sh"]

" {{{2 vim-dispatch
" don't touch my mappings
let g:nremap = {"m": "", "`": "", "'": "", "g'": ""}


" {{{2 fzf
" Use a full (new empty) buffer for fzf commands
let g:fzf_layout = { 'window': 'enew' }
" nicer git commmit format (:Commits)
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

" {{{3 projectionist
" augroup configure_projects
"   autocmd!
"   autocmd User ProjectionistActivate call s:compilers()
" augroup END
" 
" function! s:compilers() abort
"   let l:compile_command = projectionist#query('dispatch')
"   if len(l:compile_command) > 0
"     nnoremap <buffer> <leader>pc :Dispatch<CR>
"   endif
" " nnoremap <buffer> <leader>pt :Dispatch cargo run<CR>
" " nnoremap <buffer> <leader>pf :Dispatch cargo fmt<CR>
" endfunction


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

  " make nvr the commit message editor
  " https://github.com/mhinz/neovim-remote/blob/master/README.md
  let $VISUAL = 'nvr -cc split --remote-wait'

  " much nicer :s usage (with preview)
  set inccommand=nosplit

  " This is from my tmux time
  nnoremap <A-right> :tabnext<CR>
  nnoremap <A-left> :tabprevious<CR>
endif


" {{{1 Local (specific) extra vimrc
"-------------------------------------------------------------------------------
let s:local_vimrc = expand('$HOME').'/.vimrc-extra'
if filereadable(s:local_vimrc)
  execute 'source' s:local_vimrc
endif

" {{{1 modeline
" vim: fdm=marker
