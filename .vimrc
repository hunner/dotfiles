
"-----------------------------------------------------------------------
"Hunner's vimrc based on BaSS & ciaran
"-----------------------------------------------------------------------

"-----------------------------------------------------------------------
" terminal setup
"-----------------------------------------------------------------------
" {{{1

" This may contain utf-8 script
scriptencoding utf-8

" Want utf8 at all times
set termencoding=utf-8
set encoding=utf-8
set fileencoding=utf-8

" change cursor colour depending upon mode
if exists('&t_SI')
  let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
  let &t_EI = "\<Esc>]12;green\x7"
elseif has("gui")
  set guicursor=n-v-c:block-Cursor/lCursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor,sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
endif

" Clear autocommands for re-sourceing
autocmd!


" }}}1

"-----------------------------------------------------------------------
" settings
"-----------------------------------------------------------------------
" {{{1

" Don't be compatible with vi {{{2
set nocompatible

" Enable a nice big viminfo file {{{2
set viminfo='1000,f1,:1000,/1000
set history=500

" Return to last line on reopening file {{{2
if has("autocmd")
  autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" Abbreviate output of commands {{{2
set shortmess=a

" Make backspace delete lots of things {{{2
set backspace=indent,eol,start

" Don't create backups {{{2
"set nobackup

" Show us the command we're typing {{{2
set showcmd

" Highlight matching parens {{{2
set showmatch

" Search options: incremental search, highlight search {{{2
set hlsearch
set incsearch

" Case insensitivity for searching {{{2
set ignorecase
set infercase

" Show full tags when doing search completion {{{2
set showfulltag

" Speed up macros with lazyredraw {{{2
set lazyredraw

" No annoying error noises {{{2
set noerrorbells
set visualbell t_vb=
if has("autocmd")
  autocmd GUIEnter * set visualbell t_vb=
endif

" Scroll buffers of 3x2 {{{2
set scrolloff=3
set sidescrolloff=2

" Wrap on < > [ ] too {{{2
set whichwrap+=<,>,[,]

" Use the cool tab complete wildmenu {{{2
set wildmenu
set wildignore+=*.o,*~,.lo
set suffixes+=.in,.a,.1

" Allow edit buffers to be hidden {{{2
set hidden

" Enable syntax highlighting {{{2
if has("syntax")
  syntax on
endif

" enable virtual edit in vblock mode, and one past the end {{{2
set virtualedit=block

" Set our fonts {{{2
"if has("gui_kde")
"  set guifont=Terminus/12/-1/5/50/0/0/0/0/0
"elseif has("gui_gtk")
"  set guifont=Terminus\ 12
"elseif has("gui_running")
"  set guifont=-xos4-terminus-medium-r-normal--12-140-72-72-c-80-iso8859-1
"endif

" Try to load a nice colourscheme {{{2
if ! has("gui_running")
  set t_Co=256
  colors inkpot
else
  colors ir_black
  " Turn off the menubar so we don't get key accelerators with Meta.
  " Don't include the toolbar
  set guioptions=aegit
endif
" set background=light gives a different style, feel free to choose between them.
set background=dark
"colors peaksea

" No icky toolbar, menu or scrollbars in the GUI {{{2
"if has('gui')
"    set guioptions-=m
"    set guioptions-=T
"    set guioptions-=l
"    set guioptions-=L
"    set guioptions-=r
"    set guioptions-=R
"end

" By default, go for an indent of 4 and use spaces {{{2
set expandtab
set shiftwidth=4
set tabstop=4

" Do clever indent things. Don't make a # force column zero. {{{2
set autoindent
set smartindent
inoremap # X<BS>#

" Disable folds by default; toggle with zi {{{2
if has("folding")
  fun! ToggleFoldmethod()
    if &foldmethod == "marker"
      set foldmethod=syntax
    else
      set foldmethod=marker
    endif
  endfun
  command! Tfdm call ToggleFoldmethod()
  set nofoldenable
  set foldmethod=syntax
  set foldlevelstart=0 " Start with all folds closed
  "set foldclose=all " Close folds when cursor leaves them
endif

" Syntax when printing {{{2
set popt+=syntax:y

" Enable filetype settings {{{2
if has("eval")
  filetype on
  filetype plugin on
  filetype indent on
endif

" Enable modelines only on secure vim versions {{{2
if (v:version >= 604)
  set modeline
else
  set nomodeline
endif

" Nice statusbar {{{2
set laststatus=2
set statusline=
set statusline+=%2*%-3.3n%0*\                " buffer number
set statusline+=%f\                          " file name
if has("eval")
  let g:scm_cache = {}
  fun! ScmInfo()
    let l:key = getcwd()
    if ! has_key(g:scm_cache, l:key)
      if (isdirectory(getcwd() . "/.git"))
        let g:scm_cache[l:key] = "[" . substitute(readfile(getcwd() . "/.git/HEAD", "", 1)[0],
              \ "^.*/", "", "") . "] "
      else
        let g:scm_cache[l:key] = ""
      endif
    endif
    return g:scm_cache[l:key]
  endfun
  set statusline+=%{ScmInfo()}             " scm info
endif
set statusline+=%h%1*%m%r%w%0*               " flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}, " filetype
set statusline+=%{&encoding},                " encoding
set statusline+=%{&fileformat}]              " file format
if filereadable(expand("$VIM/vimfiles/plugin/vimbuddy.vim"))
  set statusline+=\ %{VimBuddy()}          " vim buddy
endif
set statusline+=%=                           " right align
set statusline+=%2*0x%-8B\                   " current char
set statusline+=%-14.(%l,%c%V%)\ %<%P        " offset

" special statusbar for special windows
"if has("autocmd")
"    au FileType qf
"                \ if &buftype == "quickfix" |
"                \     setlocal statusline=%2*%-3.3n%0* |
"                \     setlocal statusline+=\ \[Compiler\ Messages\] |
"                \     setlocal statusline+=%=%2*\ %<%P |
"                \ endif
"
"    fun! <SID>FixMiniBufExplorerTitle()
"        if "-MiniBufExplorer-" == bufname("%")
"            setlocal statusline=%2*%-3.3n%0*
"            setlocal statusline+=\[Buffers\]
"            setlocal statusline+=%=%2*\ %<%P
"        endif
"    endfun
"
"    au BufWinEnter *
"                \ let oldwinnr=winnr() |
"                \ windo call <SID>FixMiniBufExplorerTitle() |
"                \ exec oldwinnr . " wincmd w"
"endif

" Nice window title {{{2
if has('title') && (has('gui_running') || &title)
  set titlestring=
  set titlestring+=%f\                                              " file name
  set titlestring+=%h%m%r%w                                         " flags
  "set titlestring+=\ -\ %{v:progname}                               " program name
  set titlestring+=\ -\ %{substitute(getcwd(),\ $HOME,\ '~',\ '')}  " working directory
endif

" Backups and undos across edits {{{2
if v:version >= 702
  set backupdir=~/.vim/backups
endif
" NB: :help usr_32.txt or undo-branches
if v:version >= 703
  set undodir=~/.vim/backups
  set undofile
endif

" Use blowfish for :X encryption {{{2
if v:version >= 703
  set cryptmethod=blowfish
endif

" If possible, try to use a narrow number column. {{{2
if v:version >= 700
  try
    setlocal numberwidth=3
  catch
  endtry
endif

" Include $HOME in cdpath {{{2
if has("file_in_path")
  let &cdpath=','.expand("$HOME").','.expand("$HOME").'/work'
endif

" Better include path {{{2
set path+=src/,include/
let &inc.=' ["<]'

" Show tabs and trailing whitespace visually {{{2
if (&termencoding == "utf-8") || has("gui_running")
  if v:version >= 700
    set list listchars=tab:»·,trail:·,extends:…,nbsp:‗
  else
    set list listchars=tab:»·,trail:·,extends:…
  endif
else
  if v:version >= 700
    set list listchars=tab:>-,trail:.,extends:>,nbsp:_
  else
    set list listchars=tab:>-,trail:.,extends:>
  endif
endif
map <silent> <F9> :set noet<CR>:set sw=8<CR>:set ts=8<CR>
map <silent> <S-F9> :set list! listchars<CR>

" Show lines longer than 80 characters {{{2
"au BufWinEnter * let w:m1=matchadd('Search', '\%<81v.\%>77v', -1)
"au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" Fill folds with ' ' {{{2
set fillchars=fold:\ 


" }}}1

"-----------------------------------------------------------------------
" completion
"-----------------------------------------------------------------------
" {{{1

set dictionary=/usr/share/dict/words


" }}}1

"-----------------------------------------------------------------------
" miniBufExpl
"-----------------------------------------------------------------------
" {{{1

"let g:miniBufExplMapWindowNavVim = 1
"let g:miniBufExplMapWindowNavArrows = 1
"let g:miniBufExplMapCTabSwitchBufs = 1
"let g:miniBufExplModSelTarget = 1 


" }}}1

"-----------------------------------------------------------------------
" autocmds
"-----------------------------------------------------------------------
" {{{1

" Show the column and/or line of the cursor {{{2
au VimEnter,BufEnter,WinEnter * set cursorcolumn " cursorline
au WinLeave * set nocursorcolumn " nocursorline

" content creation {{{2
if has("autocmd")
  augroup puppet " {{{3
    autocmd BufRead,BufNewFile *.pp
          \ set tabstop=2 shiftwidth=2 softtabstop=2
  augroup END
  augroup text " {{{3
    autocmd BufRead,BufNewFile *.txt
          \ set nonumber tw=80
  augroup END
  augroup helphelp " {{{3
    " For help files, move them to the top window and make <Return>
    " behave like <C-]> (jump to tag)
    "autocmd FileType help :call <SID>WindowToTop()
    autocmd FileType help nmap <buffer> <Return> <C-]>
  augroup END
  augroup interplangs " {{{3
    autocmd BufNewFile *.rb 0put ='# vim: set sw=2 sts=2 et tw=80 :' |
          \ 0put ='#!/usr/bin/env ruby' | set sw=2 sts=2 et tw=80 |
          \ norm G

    autocmd BufNewFile,BufRead *.rb,*rhtml,*haml
          \ set tabstop=2 shiftwidth=2 softtabstop=2 |
          \ setf eruby

    autocmd BufNewFile,BufRead *.php
          \ set ai
  augroup END
  augroup tex " {{{3
    autocmd BufNewFile *.tex
          \ 0put ='% vim:set ft=tex spell:'
  augroup END
  augroup html " {{{3
    autocmd BufNewFile *.htm,*.html
          \ 0put ='<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">' |
          \ $put ='<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">' |
          \ $put ='  <head>' |
          \ $put ='    <title></title>' |
          \ $put ='    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />' |
          \ $put ='    <link href=\"\" rel=\"stylesheet\" type=\"text/css\" />' |
          \ $put ='    <style type=\"text/css\">' |
          \ $put ='    </style>' |
          \ $put ='  </head>' |
          \ $put ='  <body>' |
          \ $put ='  </body>' |
          \ $put ='</html>' |
          \ $put ='<!-- vim: set sw=2 sts=2 et tw=80 : -->' |
          \ set sw=2 sts=2 et tw=80 | norm G
  augroup END
  augroup autotools " {{{3
    autocmd BufNewFile *.hh 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
          \ 1put ='' | call MakeIncludeGuards() |
          \ 5put ='#include \"config.h\"' |
          \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.c 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
          \ 1put ='' | 2put ='' | call setline(3, '#include "' .
          \ substitute(expand("%:t"), ".c$", ".h", "") . '"') |
          \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile *.cc 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
          \ 1put ='' | 2put ='' | call setline(3, '#include "' .
          \ substitute(expand("%:t"), ".cc$", ".hh", "") . '"') |
          \ set sw=4 sts=4 et tw=80 | norm G

    autocmd BufNewFile configure.ac
          \ 0put ='dnl vim: set sw=8 sts=8 noet :' |
          \ $put ='' |
          \ call setline(line('$'), 'AC_INIT([' . substitute(expand('%:p:h'),
          \     '^.\{-}/\([^/]\+\)\(/trunk\)\?$', '\1', '') . '], [0.1], [h.haugen@gmail.com])') |
          \ $put ='AC_PREREQ(2.63)' |
          \ $put ='AC_CONFIG_SRCDIR([])' |
          \ $put ='AC_CONFIG_AUX_DIR(config)' |
          \ $put ='AM_INIT_AUTOMAKE([foreign -Wall -Werror 1.10])' |
          \ $put ='' |
          \ $put ='dnl check for required programs' |
          \ $put ='AC_PROG_CC dnl CXX' |
          \ $put ='AC_PROG_INSTALL' |
          \ $put ='AC_PROG_LN_S' |
          \ $put ='AC_PROG_RANLIB' |
          \ $put ='AC_PROG_MAKE_SET' |
          \ $put ='' |
          \ $put ='dnl output' |
          \ $put ='AC_CONFIG_HEADERS([config.h])' |
          \ $put ='AC_CONFIG_FILES([' |
          \ $put ='	Makefile' |
          \ $put ='	src/Makefile' |
          \ $put ='])' |
          \ $put ='AC_OUTPUT' |
          \ set sw=8 sts=8 noet |
          \ norm ggjjjjf]

    autocmd BufNewFile autogen.bash
          \ 0put ='#!/usr/bin/env bash' |
          \ 1put ='# vim: set sw=4 sts=4 et tw=80 :' |
          \ $put ='run() {' |
          \ $put ='echo \">>> $@\"' |
          \ $put ='    if ! $@ ; then' |
          \ $put ='        echo \"oops!\" 1>&2' |
          \ $put ='        exit 127' |
          \ $put ='    fi' |
          \ $put ='}' |
          \ $put ='' |
          \ $put ='get() {' |
          \ $put ='    type ${1}-${2}    &>/dev/null && echo ${1}-${2}    && return' |
          \ $put ='    type ${1}${2//.}  &>/dev/null && echo ${1}${2//.}  && return' |
          \ $put ='    type ${1}         &>/dev/null && echo ${1}         && return' |
          \ $put ='    echo \"Could not find ${1} ${2}\" 1>&2' |
          \ $put ='    exit 127' |
          \ $put ='}' |
          \ $put ='' |
          \ $put ='run mkdir -p config' |
          \ $put ='run $(get libtoolize 2.2 ) --copy --force --automake' |
          \ $put ='rm -f config.cache' |
          \ $put ='run $(get aclocal 1.10 )' |
          \ $put ='run $(get autoheader 2.63 )' |
          \ $put ='run $(get autoconf 2.63 )' |
          \ $put ='run $(get automake 1.10 ) -a --copy' |
          \ set sw=4 sts=4 et tw=80 |
          \ norm gg=Ggg
    autocmd BufWritePost autogen.bash !chmod 744 %

    autocmd BufNewFile Makefile.am
          \ 0put ='CLEANFILES = *~' |
          \ if (! filereadable(expand("%:p:h:h") . '/Makefile.am')) |
          \     $put ='MAINTAINERCLEANFILES = Makefile.in configure config/* aclocal.m4 \' |
          \     $put ='' |
          \     call setline(line('$'), "\t\t\tconfig.h config.h.in") |
          \     $put ='SUBDIRS = src' |
          \     $put ='AUTOMAKE_OPTIONS = foreign dist-bzip2' |
          \     $put ='EXTRA_DIST = autogen.bash' |
          \     $put ='' |
          \     $put ='maintainer-clean-local:' |
          \     $put ='	-rmdir config' |
          \ else |
          \     $put ='MAINTAINERCLEANFILES = Makefile.in' |
          \     $put ='bin_PROGRAMS = ' . substitute(expand('%:p:h'),'^.\{-}/\([^/]\+\)\(/src\)\?$', '\1', '') |
          \     $put = substitute(expand('%:p:h'), '^.\{-}/\([^/]\+\)\(/src\)\?$','\1', '') . '_SOURCES = main.c' |
          \ endif

  augroup END
  augroup making " {{{3
    try
      " if we have a vim which supports QuickFixCmdPost (vim7),
      " give us an error window after running make, grep etc, but
      " only if results are available.
      autocmd QuickFixCmdPost * botright cwindow 6

      autocmd QuickFixCmdPre make
            \ let g:make_start_time=localtime()

      let g:paludis_configure_command = "! ./configure --prefix=/usr --sysconfdir=/etc" .
            \ " --localstatedir=/var/lib --enable-qa " .
            \ " --enable-ruby --enable-python --enable-vim --enable-bash-completion" .
            \ " --enable-zsh-completion --with-repositories=all --with-clients=all --with-environments=all" .
            \ " --enable-visibility --enable-gnu-ldconfig --enable-htmltidy" .
            \ " --enable-ruby-doc --enable-python-doc --enable-xml"

      " Similarly, try to automatically run ./configure and / or
      " autogen if necessary.
      autocmd QuickFixCmdPre make
            \ if ! filereadable('Makefile') |
            \     if ! filereadable("configure") |
            \         if filereadable("autogen.bash") |
            \             exec "! ./autogen.bash" |
            \         elseif filereadable("quagify.sh") |
            \             exec "! ./quagify.sh" |
            \         endif |
            \     endif |
            \     if filereadable("configure") |
            \         if (isdirectory(getcwd() . "/paludis/util")) |
            \             exec g:paludis_configure_command |
            \         elseif (match(getcwd(), "libwrapiter") >= 0) |
            \             exec "! ./configure --prefix=/usr --sysconfdir=/etc" |
            \         else |
            \             exec "! ./configure" |
            \         endif |
            \     endif |
            \ endif

      autocmd QuickFixCmdPost make
            \ let g:make_total_time=localtime() - g:make_start_time |
            \ echo printf("Time taken: %dm%2.2ds", g:make_total_time / 60,
            \     g:make_total_time % 60)

      autocmd QuickFixCmdPre *
            \ let g:old_titlestring=&titlestring |
            \ let &titlestring="[ " . expand("<amatch>") . " ] " . &titlestring |
            \ redraw

      autocmd QuickFixCmdPost *
            \ let &titlestring=g:old_titlestring

      if hostname() == "snowmobile"
        autocmd QuickFixCmdPre make
              \ let g:active_line=getpid() . " vim:" . substitute(getcwd(), "^.*/", "", "") |
              \ exec "silent !echo '" . g:active_line . "' >> ~/.config/awesome/active"

        autocmd QuickFixCmdPost make
              \ exec "silent !sed -i -e '/^" . getpid() . " /d' ~/.config/awesome/active"
      endif

    catch
    endtry
  augroup END
endif


" Preview window for :help CursorHold-example after updatetime {{{2
au CursorHold *.[ch] nested call PreviewWord()
au CursorMoved *.[ch] nested call UnPreviewWord()
fun! UnPreviewWord()
  if &previewwindow
    return
  endif
  pclose
endfun
fun! PreviewWord()
  if &previewwindow                  " don't do this in the preview window
    return
  endif
  if &lines < 40                     " not for small terminals
    return
  endif
  let w = expand("<cword>")          " get the word under cursor
  if w =~ '\a'                       " if the word contains a letter
    " Delete any existing highlight before showing another tag
    silent! wincmd P                 " jump to preview window
    if &previewwindow                " if we really get there...
      match none                     " delete existing highlight
      wincmd p                       " back to old window
    endif

    " Try displaying a matching tag for the word under the cursor
    try
      exe "ptag " . w
    catch
      return
    endtry

    silent! wincmd P                 " jump to preview window
    if &previewwindow                " if we really get there...
"     exe "wincmd J"                 " make the window appear below
      if has("folding")
        silent! .foldopen            " don't want a closed fold
      endif
      call search("$", "b")          " to end of previous line
      let w = substitute(w, '\\', '\\\\', "")
      call search('\<\V' . w . '\>') " position cursor on match
      " Add a match highlight to the word at this position
      hi previewWord term=bold cterm=underline gui=underline
      exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
"     exe "normal " . &previewheight / 2 . "j"
      wincmd p                       " back to old window
    endif
  endif
endfun


" }}}1

"-----------------------------------------------------------------------
" mappings
"-----------------------------------------------------------------------
" {{{1

" Go to buffers with S-left/right and ^w ,. {{{2
nmap <silent> <S-Left>  :bprev<CR>
nmap <silent> <S-Right> :bnext<CR>
nmap <C-w>, :bprev<CR>
nmap <C-w>. :bnext<CR>

" Movement between windows with ^hjkl {{{2
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Move through buffers instead of tabs with gt/gT {{{2
"nmap gT :bprev<CR>
"nmap gt :bnext<CR>

" v_K is really really annoying; disable {{{2
vmap K k

" Puppet pkg sort with <L>sp {{{2
nmap <Leader>sp vi[:sort<CR>

" Delete a buffer but keep layout with ^w! {{{2
if has("eval")
  command! Kwbd enew|bw #
  nmap     <C-w>!   :Kwbd<CR>
endif

" quickfix things like '-' {{{2
"nmap <Leader>cwc :cclose<CR>
"nmap <Leader>cwo :botright copen 5<CR><C-w>p
"nmap <Leader>cn  :cnext<CR>
"nmap <Leader>cp  :cprevious<CR>
nmap -  :cnext<CR>
nmap _  :cprev<CR>
nmap <C--> :colder<CR>
nmap <C-_> :cnewer<CR>

" Make S-up/down do gk/gj {{{2
inoremap <S-Up>   <C-o>gk
inoremap <S-Down> <C-o>gj
noremap  <S-Up>   gk
noremap  <S-Down> gj

" Better Bépo movement {{{2
noremap © h
noremap þ j
noremap ß k
noremap ® l

" Make <space>/<backspace> page up/down {{{2
noremap <space> <C-f>
noremap <backspace> <C-b>

" Scrolling with arrows controls the window {{{2
noremap <Up>   <C-y>
noremap <Down> <C-e>

" Useful things from inside imode {{{2
inoremap <C-z>w <C-o>:w<CR>
inoremap <C-z>q <C-o>gq}<C-o>k<C-o>$

" Commonly used commands {{{2
"nmap <silent> <F3> :silent nohlsearch<CR>
"imap <silent> <F3> <C-o>:silent nohlsearch<CR>
"nmap <F4> :Kwbd<CR>
"nmap <F5> <C-w>c
"nmap <F6> :exec "make check TESTS_ENVIRONMENT=true LOG_COMPILER=true XFAIL_TESTS="<CR>
"nmap <Leader><F6> :exec "make -C " . expand("%:p:h") . " check TESTS_ENVIRONMENT=true LOG_COMPILER=true XFAIL_TESTS="<CR>
"nmap <F7> :make all-then-check<CR>
map <F7> :Tfdm<CR>
"nmap <Leader><F7> :exec "make -C " . expand("%:p:h") . " check"<CR>
nmap <F8> :make<CR>
nmap <Leader><F8> :exec "make -C " . expand("%:p:h")<CR>
"nmap <F9> :exec "make -C " . expand("%:p:h") . " check SUBDIRS= check_PROGRAMS=" . GetCurrentTest()
"            \ . " TESTS=" . GetCurrentTest() <CR>

" Insert a single char {{{2
noremap <Leader>i i<Space><Esc>r

" Split the line into a (n)ew line or an (o)pen line {{{2
nmap <Leader>n \i<CR>
nmap <Leader>o \i<CR>k$

" Pull the following line to the cursor position {{{2
noremap <Leader>J :s/\%#\(.*\)\n\(.*\)/\2\1<CR>

" In normal mode, jj or jl escapes {{{2
inoremap jj <Esc>
inoremap jl <Esc>

" Kill line like emacs {{{2
"noremap <C-k> "_dd

" Select everything {{{2
noremap <Leader>gg ggVG

" Reformat everything {{{2
noremap <Leader>gq gggqG

" Reformat paragraph {{{2
noremap <Leader>gp gqap

" Clear lines {{{2
"noremap <Leader>clr :s/^.*$//<CR>:nohls<CR>

" Delete blank lines {{{2
noremap <Leader>dbl :g/^$/d<CR>:nohls<CR>

" Enclose each selected line with markers {{{2
noremap <Leader>enc :<C-w>execute
      \ substitute(":'<,'>s/^.*/#&#/ \| :nohls", "#", input(">"), "g")<CR>

" Edit something in the current directory {{{2
noremap <Leader>ed :e <C-r>=expand("%:p:h")<CR>/<C-d>

" Enable fancy % matching {{{2
if has("eval")
  runtime! macros/matchit.vim
endif

" q: sucks {{{2
"nmap q: :q

" set up some more useful digraphs {{{2
if has("digraphs")
  digraph ., 8230    " ellipsis (…)
endif

" What does this do? {{{2
if has("eval")
  " Work out include guard text
  fun! IncludeGuardText()
    let l:p = substitute(substitute(getcwd(), "/trunk", "", ""), '^.*/', "", "")
    let l:t = substitute(expand("%"), "[./]", "_", "g")
    return substitute(toupper(l:p . "_GUARD_" . l:t), "-", "_", "g")
  endfun

  " Make include guards
  fun! MakeIncludeGuards()
    norm gg
    /^$/
    norm 2O
    call setline(line("."), "#ifndef " . IncludeGuardText())
    norm o
    call setline(line("."), "#define " . IncludeGuardText() . " 1")
    norm G
    norm o
    call setline(line("."), "#endif")
  endfun
  noremap <Leader>ig :call MakeIncludeGuards()<CR>
endif

" javascript folding {{{2
if has("eval")
  function! JavaScriptFold()
    setl foldmethod=syntax
    setl foldlevelstart=1
    syn region foldBraces start=/{/ end=/}/ transparent fold keepend extend

    function! FoldText()
      return substitute(getline(v:foldstart), '{.*', '{...}', '')
    endfunction
    setl foldtext=FoldText()
  endfunction
  au FileType javascript call JavaScriptFold()
  au FileType javascript setl fen
endif

" fast buffer switching {{{2
if v:version >= 700 && has("eval")
  let g:switch_header_map = {
        \ 'cc':    'hh',
        \ 'hh':    'cc',
        \ 'c':     'h',
        \ 'h':     'c',
        \ 'cpp':   'hpp',
        \ 'hpp':   'cpp' }

  fun! SwitchTo(f, split) abort
    if ! filereadable(a:f)
      echoerr "File '" . a:f . "' does not exist"
    else
      if a:split
        new
      endif
      if 0 != bufexists(a:f)
        exec ':buffer ' . bufnr(a:f)
      else
        exec ':edit ' . a:f
      endif
    endif
  endfun

  fun! SwitchHeader(split) abort
    let filename = expand("%:p:r")
    let suffix = expand("%:p:e")
    if suffix == ''
      echoerr "Cannot determine header file (no suffix)"
      return
    endif

    let new_suffix = g:switch_header_map[suffix]
    if new_suffix == ''
      echoerr "Don't know how to find the header (suffix is " . suffix . ")"
      return
    end

    call SwitchTo(filename . '.' . new_suffix, a:split)
  endfun

  fun! SwitchTest(split) abort
    let filename = expand("%:p:r")
    let suffix = expand("%:p:e")
    if -1 != match(filename, '_TEST$')
      let new_filename = substitute(filename, '_TEST$', '.' . suffix, '')
    else
      let new_filename = filename . '_TEST.' . suffix
    end
    call SwitchTo(new_filename, a:split)
  endfun

  fun! SwitchMakefile(split) abort
    let dirname = expand("%:p:h")
    if filereadable(dirname . "/Makefile.am.m4")
      call SwitchTo(dirname . "/Makefile.am.m4", a:split)
    elseif filereadable(dirname . "/Makefile.am")
      call SwitchTo(dirname . "/Makefile.am", a:split)
    else
      call SwitchTo(dirname . "/Makefile", a:split)
    endif
  endfun

  noremap <Leader>sh :call SwitchHeader(0)<CR>
  noremap <Leader>st :call SwitchTest(0)<CR>
  noremap <Leader>sk :call SwitchMakefile(0)<CR>
  noremap <Leader>ssh :call SwitchHeader(1)<CR>
  noremap <Leader>sst :call SwitchTest(1)<CR>
  noremap <Leader>ssk :call SwitchMakefile(1)<CR>
endif

" super i_c-y / i_c-e {{{2
if v:version >= 700 && has("eval")
  fun! SuperYank(offset)
    let l:cursor_pos = col(".")
    let l:this_line = line(".")
    let l:source_line = l:this_line + a:offset
    let l:this_line_text = getline(l:this_line)
    let l:source_line_text = getline(l:source_line)
    let l:add_text = ""

    let l:motion = "" . nr2char(getchar())
    if -1 != match(l:motion, '\d')
      let l:count = 0
      while -1 != match(l:motion, '\d')
        let l:count = l:count * 10 + l:motion
        let l:motion = "" . nr2char(getchar())
      endwhile
    else
      let l:count = 1
    endif

    if l:motion == "$"
      let l:add_text = strpart(l:source_line_text, l:cursor_pos - 1)
    elseif l:motion == "w"
      let l:add_text = strpart(l:source_line_text, l:cursor_pos - 1)
      let l:add_text = substitute(l:add_text,
            \ '^\(\s*\%(\S\+\s*\)\{,' . l:count . '}\)\?.*', '\1', '')
    elseif l:motion == "f" || l:motion == "t"
      let l:add_text = strpart(l:source_line_text, l:cursor_pos - 1)
      let l:char = nr2char(getchar())
      let l:pos = matchend(l:add_text,
            \ '^\%([^' . l:char . ']\{-}' . l:char . '\)\{' . l:count . '}')
      if -1 != l:pos
        let l:add_text = strpart(l:add_text, 0, l:motion == "f" ? l:pos : l:pos - 1)
      else
        let l:add_text = ''
      endif
    else
      echo "Unknown motion: " . l:motion
    endif

    if l:add_text != ""
      let l:new_text = strpart(l:this_line_text, 0, l:cursor_pos - 1) .
            \ l:add_text . strpart(l:this_line_text, l:cursor_pos - 1)
      call setline(l:this_line, l:new_text)
      call cursor(l:this_line, l:cursor_pos + strlen(l:add_text))
    endif
  endfun

  inoremap <C-g>y <C-\><C-o>:call SuperYank(-1)<CR>
  inoremap <C-g>e <C-\><C-o>:call SuperYank(1)<CR>
endif

" tab completion {{{2
if has("eval")
  function! CleverTab()
    if strpart(getline('.'), 0, col('.') - 1) =~ '^\s*$'
      return "\<Tab>"
    else
      return "\<C-N>"
    endif
  endfun
  inoremap <Tab> <C-R>=CleverTab()<CR>
  inoremap <S-Tab> <C-P>
endif

" ^n Show number and fold columns in windows {{{2
if has("eval")
  function! <SID>FoldNumbers()
    " If we're in a wide window, enable line numbers.
    if winwidth(0) >= 76 " 72 + 4, or should I use tw?
      " Add folds, or cycle through number schemes
      if &foldlevel < 99 && &foldenable && &foldcolumn == 0
        setlocal foldcolumn=1
      elseif (&foldlevel == 99 || ! &foldenable) && &foldcolumn != 0
        setlocal foldcolumn=0
      elseif ! &rnu && ! &nu
        setlocal relativenumber
      elseif &rnu
        setlocal number
      elseif &nu
        setlocal nonumber
      endif
    else
      setlocal norelativenumber
      setlocal nonumber
      setlocal foldcolumn=0
    endif
  endfun
  "autocmd WinEnter,BufWinEnter,BufNew * :call <SID>FoldNumbers()
  noremap <silent> <C-n> :call <SID>FoldNumbers()<CR>
endif

" }}}1

"-----------------------------------------------------------------------
" abbreviations
"-----------------------------------------------------------------------
" {{{1

if has("eval") && has("autocmd")
  fun! <SID>abbrev_cpp()
    iabbrev <buffer> raise throw
    iabbrev <buffer> jci const_iterator
    iabbrev <buffer> jcl class
    iabbrev <buffer> jco const
    iabbrev <buffer> jdb \bug
    iabbrev <buffer> jde \throws
    iabbrev <buffer> jdf /** \file<CR><CR>/<Up>
    iabbrev <buffer> jdg \ingroup
    iabbrev <buffer> jdn /** \namespace<CR><CR>/<Up>
    iabbrev <buffer> jdp \param
    iabbrev <buffer> jdt \test
    iabbrev <buffer> jdx /**<CR><CR>/<Up>
    iabbrev <buffer> jit iterator
    iabbrev <buffer> jns namespace
    iabbrev <buffer> jpr protected
    iabbrev <buffer> jpu public
    iabbrev <buffer> jpv private
    iabbrev <buffer> jsl std::list
    iabbrev <buffer> jsm std::map
    iabbrev <buffer> jss std::string
    iabbrev <buffer> jsv std::vector
    iabbrev <buffer> jty typedef
    iabbrev <buffer> jun using namespace
    iabbrev <buffer> jvi virtual
  endfun

  augroup abbreviations
    autocmd!
    autocmd FileType cpp :call <SID>abbrev_cpp()
  augroup END
endif
" NB: Need more of these for more than cpp

" }}}1

"-----------------------------------------------------------------------
" special less.sh and man modes
"-----------------------------------------------------------------------
" {{{1

if has("eval") && has("autocmd")
  fun! <SID>check_pager_mode()
    if exists("g:loaded_less") && g:loaded_less
      " we're in vimpager / less.sh / man mode
      set laststatus=0
      set ruler
      set foldmethod=manual
      set foldlevel=99
      set nolist
    endif
  endfun
  autocmd VimEnter * :call <SID>check_pager_mode()
endif

" }}}1

"-----------------------------------------------------------------------
" plugin / script / app settings
"-----------------------------------------------------------------------
" {{{1

if has("eval")
  " Perl specific options
  let perl_include_pod=1
  let perl_fold=1
  let perl_fold_blocks=1

  " Vim specific options
  let g:vimsyntax_noerror=1
  let g:vimembedscript=0

  " c specific options
  let g:c_gnu=1
  let g:c_no_curly_error=1

  " eruby options
  au Syntax * hi link erubyRubyDelim Directory

  " ruby options
  let ruby_operators=1
  let ruby_space_errors=1

  " php specific options
  let php_sql_query=1
  let php_htmlInStrings=1

  " Settings for taglist.vim
  let Tlist_Use_Right_Window=1
  let Tlist_Auto_Open=0
  let Tlist_Enable_Fold_Column=0
  let Tlist_Compact_Format=1
  let Tlist_WinWidth=28
  let Tlist_Exit_OnlyWindow=1
  let Tlist_File_Fold_Auto_Close=1
  let Tlist_Inc_Winwidth=0
  "nnoremap <silent> <F9> :Tlist<CR>

  " Settings minibufexpl.vim
  "let g:miniBufExplModSelTarget = 1
  "let g:miniBufExplWinFixHeight = 1
  "let g:miniBufExplWinMaxSize = 1
  " let g:miniBufExplForceSyntaxEnable = 1

  " Settings for showmarks.vim
  if has("gui_running")
    let g:showmarks_enable=1
  else
    let g:showmarks_enable=0
    let loaded_showmarks=1
  endif

  let g:showmarks_include="abcdefghijklmnopqrstuvwxyz"

  if has("autocmd")
    fun! <SID>FixShowmarksColours()
      if has('gui') 
        hi ShowMarksHLl gui=bold guifg=#a0a0e0 guibg=#2e2e2e 
        hi ShowMarksHLu gui=none guifg=#a0a0e0 guibg=#2e2e2e 
        hi ShowMarksHLo gui=none guifg=#a0a0e0 guibg=#2e2e2e 
        hi ShowMarksHLm gui=none guifg=#a0a0e0 guibg=#2e2e2e 
        hi SignColumn   gui=none guifg=#f0f0f8 guibg=#2e2e2e 
      endif
    endfun
    if v:version >= 700
      autocmd VimEnter,Syntax,ColorScheme * call <SID>FixShowmarksColours()
    else
      autocmd VimEnter,Syntax * call <SID>FixShowmarksColours()
    endif
  endif

  " Settings for explorer.vim
  let g:explHideFiles='^\.'

  " Settings for netrw
  let g:netrw_list_hide='^\.,\~$'

  " Settings for :TOhtml
  let html_number_lines=1
  let html_use_css=1
  let use_xhtml=1

  " cscope settings
  if has('cscope') && filereadable("/usr/bin/cscope")
    set csto=0
    set cscopetag
    set nocsverb
    if filereadable("cscope.out")
      cs add cscope.out
    endif
    set csverb

    let x = "sgctefd"
    while x != ""
      let y = strpart(x, 0, 1) | let x = strpart(x, 1)
      exec "nmap <C-j>" . y . " :cscope find " . y .
            \ " <C-R>=expand(\"\<cword\>\")<CR><CR>"
      exec "nmap <C-j><C-j>" . y . " :scscope find " . y .
            \ " <C-R>=expand(\"\<cword\>\")<CR><CR>"
    endwhile
    nmap <C-j>i      :cscope find i ^<C-R>=expand("<cword>")<CR><CR>
    nmap <C-j><C-j>i :scscope find i ^<C-R>=expand("<cword>")<CR><CR>
  endif
endif

" }}}1

"-----------------------------------------------------------------------
" final commands (clean this cruft up -- don't add more here)
"-----------------------------------------------------------------------
" {{{1

" mio
"let Tlist_Ctags_Cmd="/usr/bin/exuberant-ctags"
" plegado ident para python
au FileType python set foldmethod=indent
" plegado syntax para sgml,htmls,xml y xsl
au Filetype html,xml,xsl,sgml ",docbook
" explorador vertical
let g:explVertical=1
" define leader como =
"let mapleader = "="

" Terminal companability
map <F15> <S-F3>
nmap <Esc>[14~ <S-F4>
nmap <Esc>[23~ <S-F1>
nmap <Esc>[24~ <S-F2>
nmap <Esc>[25~ <S-F3>
nmap <Esc>[26~ <S-F4>
nmap <Esc>[28~ <S-F5>
nmap <Esc>[29~ <S-F6>
nmap <Esc>[31~ <S-F7>
nmap <Esc>[32~ <S-F8>
nmap <Esc>[33~ <S-F9>
nmap <Esc>[34~ <S-F10>

map <F17> <S-F5>
map <F18> <S-F6>
map <F19> <S-F7>
map <F20> <S-F8>
map <F21> <S-F9>
map <F22> <S-F10>
map <F23> <S-F11>
map <F24> <S-F12>
map <S-F2> :vsplit ~/.vim/ref_full.vim<CR>
map <F2> :11vsplit ~/.vim/ref.vim<CR>
"map <F3> :Sexplore $HOME<CR>
map <S-F3> :2split ~/.vim/fun_ref.vim<CR>
map <F4> :set rnu!<CR>
map <S-F4> :set nu!<CR>
map <F5> ggVGg?
noremap <F6> :set encoding=utf-8<CR>:set fenc=utf-8<CR>
noremap <S-F6> :set encoding=iso8859-15<CR>:set fenc=iso8859-15<CR>
"map <F7> :SpellProposeAlternatives<CR>
"map <S-F7> :SpellCheck<CR>
"map <C-F7> :let spell_language_list = "english,spanish"
"nnoremap <silent> <F8> :Tlist<CR>
"nnoremap <silent> <S-F8> :TlistSync<CR>
nnoremap <Esc> :noh<CR><Esc>
map <F11> !!date<CR>
map <F12> :TC<CR>
nnoremap  :X        :x
nnoremap  :W        :w
nnoremap  :Q        :q
nnoremap  :B        :b
noremap <Leader>rg :color relaxedgreen<CR>
noremap <Leader>ip :color inkpot<CR>
noremap <Leader>ir :color ir_black<CR>
noremap <Leader>mv :color macvim<CR>:set background=light<CR>
map <Leader>f :FufFile<CR>
map <Leader>b :FufBuffer<CR>
map <Leader>c :FufDir<CR>
map <Leader>w :bdelete<CR>
map <F1> :FufHelp<CR>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>
syntax sync minlines=200

" NERD tree. Yay!
nmap <silent> <C-G> :NERDTreeToggle<CR>

" Javac
"set makeprg=javac\ %
"set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#

" Spell
"let spell_executable = "aspell"
"let spell_language_list = "spanish,english"
set spelllang=en_us,eo

" Comentiffy
let g:EnhCommentifyMultiPartBlocks = 'yes'
let g:EnhCommentifyAlignRight = 'yes'
" let g:EnhCommentifyRespectIndent = 'Yes'
let g:EnhCommentifyPretty = 'Yes'
" let g:EnhCommentifyUserBindings = 'yes'

" turn off any existing search
if has("autocmd")
  au VimEnter * nohls
endif

" }}}1

"-----------------------------------------------------------------------
"ii irc stuff
"-----------------------------------------------------------------------
" {{{1
" map c1 :.w! >> ~/irc/irc.cat.pdx.edu/in<cr>dd
" map c2 :.w! >> ~/irc/irc.cat.pdx.edu/\#hack/in<cr>dd
" map c3 :.w! >> ~/irc/irc.cat.pdx.edu/\#meow/in<cr>dd
" map c4 :.w! >> ~/irc/irc.cat.pdx.edu/\#rtttoee/in<cr>dd
" map c5 :.w! >> ~/irc/irc.cat.pdx.edu/\#robots/in<cr>dd
" map c17 :.w! >> ~/irc/irc.cat.pdx.edu/\#cschat/in<cr>dd
" imap /me <C-V><C-A>ACTION

" }}}1

"-----------------------------------------------------------------------
" vim: set sw=2 sts=2 et tw=72 fdm=marker:
