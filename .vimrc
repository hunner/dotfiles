scriptencoding utf-8

"-----------------------------------------------------------------------
"Hunner's vimrc based on BaSS & ciaran
"-----------------------------------------------------------------------

"-----------------------------------------------------------------------
" terminal setup
"-----------------------------------------------------------------------

" Want utf8 at all times
set termencoding=utf-8
set encoding=utf-8
set fenc=utf-8

" change cursor colour depending upon mode
if exists('&t_SI')
    let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
    let &t_EI = "\<Esc>]12;green\x7"
endif

"-----------------------------------------------------------------------
" settings
"-----------------------------------------------------------------------

" Don't be compatible with vi
set nocompatible

" Enable a nice big viminfo file
set viminfo='1000,f1,:1000,/1000
set history=500

" Return to last line
if has("autocmd")
    autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" Abbreviate output of commands
set shortmess=a

" Make backspace delete lots of things
set backspace=indent,eol,start

" Don't create backups
set nobackup

" Show us the command we're typing
set showcmd

" Highlight matching parens
set showmatch

" Search options: incremental search, highlight search
set hlsearch
set incsearch

" Case insensitivity
set ignorecase
set infercase

" Show full tags when doing search completion
set showfulltag

" Speed up macros
set lazyredraw

" No annoying error noises
set noerrorbells
set visualbell t_vb=
if has("autocmd")
    autocmd GUIEnter * set visualbell t_vb=
endif

" Try to show at least three lines and two columns of context when
" scrolling
set scrolloff=3
set sidescrolloff=2

" Wrap on these
set whichwrap+=<,>,[,]

" Use the cool tab complete menu
set wildmenu
set wildignore+=*.o,*~,.lo
set suffixes+=.in,.a,.1

" Allow edit buffers to be hidden
set hidden

" 1 height windows
set winminheight=1

" Enable syntax highlighting
if has("syntax")
    syntax on
endif

" enable virtual edit in vblock mode, and one past the end
set virtualedit=block,onemore

" Set our fonts
if has("gui_kde")
    set guifont=Terminus/12/-1/5/50/0/0/0/0/0
elseif has("gui_gtk")
    set guifont=Terminus\ 12
elseif has("gui_running")
    set guifont=-xos4-terminus-medium-r-normal--12-140-72-72-c-80-iso8859-1
endif

" Try to load a nice colourscheme
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

" No icky toolbar, menu or scrollbars in the GUI
"if has('gui')
"    set guioptions-=m
"    set guioptions-=T
"    set guioptions-=l
"    set guioptions-=L
"    set guioptions-=r
"    set guioptions-=R
"end

" By default, go for an indent of 4 and use spaces
set expandtab
set shiftwidth=4
set tabstop=4

" Do clever indent things. Don't make a # force column zero.
set autoindent
set smartindent
inoremap # X<BS>#

" Enable folds
if has("folding")
    set foldenable
    set foldmethod=manual
    set foldlevelstart=99
endif

" Syntax when printing
set popt+=syntax:y

" Enable filetype settings
if has("eval")
    filetype on
    filetype plugin on
    filetype indent on
endif

" Enable modelines only on secure vim versions
if (v:version >= 604)
    set modeline
else
    set nomodeline
endif

" Nice statusbar
set laststatus=2
set statusline=
set statusline+=%2*%-3.3n%0*\                " buffer number
set statusline+=%f\                          " file name
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
if has("autocmd")
    au FileType qf
                \ if &buftype == "quickfix" |
                \     setlocal statusline=%2*%-3.3n%0* |
                \     setlocal statusline+=\ \[Compiler\ Messages\] |
                \     setlocal statusline+=%=%2*\ %<%P |
                \ endif

    fun! <SID>FixMiniBufExplorerTitle()
        if "-MiniBufExplorer-" == bufname("%")
            setlocal statusline=%2*%-3.3n%0*
            setlocal statusline+=\[Buffers\]
            setlocal statusline+=%=%2*\ %<%P
        endif
    endfun

    au BufWinEnter *
                \ let oldwinnr=winnr() |
                \ windo call <SID>FixMiniBufExplorerTitle() |
                \ exec oldwinnr . " wincmd w"
endif

" Nice window title
if has('title') && (has('gui_running') || &title)
    set titlestring=
    set titlestring+=%f\                     " file name
    set titlestring+=%h%m%r%w                " flags
    set titlestring+=\ -\ %{v:progname}      " program name
endif

" If possible, try to use a narrow number column.
if v:version >= 700
    try
        setlocal numberwidth=3
    catch
    endtry
endif

" Include $HOME in cdpath
if has("file_in_path")
    let &cdpath=','.expand("$HOME").','.expand("$HOME").'/work'
endif

" Better include path
set path+=src/

" Show tabs and trailing whitespace visually
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
map <F9> :set nolist listchars<CR>

" Show lines longer than 80 characters
"au BufWinEnter * let w:m1=matchadd('Search', '\%<81v.\%>77v', -1)
"au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

set fillchars=fold:-

"-----------------------------------------------------------------------
" completion
"-----------------------------------------------------------------------
set dictionary=/usr/share/dict/words


"-----------------------------------------------------------------------
" miniBufExpl
"-----------------------------------------------------------------------
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1 


"-----------------------------------------------------------------------
" autocmds
"-----------------------------------------------------------------------

if has("eval")

    " If we're in a wide window, enable line numbers.
    fun! <SID>WindowWidth()
        if winwidth(0) > 90
            setlocal foldcolumn=1
            setlocal number
        else
            setlocal nonumber
            setlocal foldcolumn=0
        endif
    endfun
endif

" content creation
if has("autocmd")
    augroup content
        autocmd!

        autocmd BufNewFile *.rb 0put ='# vim: set sw=2 sts=2 et tw=80 :' |
                    \ 0put ='#!/usr/bin/env ruby' | set sw=2 sts=2 et tw=80 |
                    \ norm G

        autocmd BufNewFile,BufRead *.rb,*rhtml,*haml
                    \ set tabstop=2 shiftwidth=2 softtabstop=2 |
                    \ setf eruby

        autocmd BufNewFile,BufRead *.php
                    \ set ai

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

        autocmd BufNewFile *.hh 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                    \ 1put ='' | call MakeIncludeGuards() |
                    \ 5put ='#include \"config.h\"' |
                    \ set sw=4 sts=4 et tw=80 | norm G

        autocmd BufNewFile *.cc 0put ='/* vim: set sw=4 sts=4 et foldmethod=syntax : */' |
                    \ 1put ='' | 2put ='' | call setline(3, '#include "' .
                    \ substitute(expand("%:t"), ".cc$", ".hh", "") . '"') |
                    \ set sw=4 sts=4 et tw=80 | norm G

        autocmd BufNewFile configure.ac
                    \ 0put ='dnl vim: set sw=8 sts=8 noet :' |
                    \ $put ='' |
                    \ call setline(line('$'), 'AC_INIT([' . substitute(expand('%:p:h'),
                    \     '^.\{-}/\([^/]\+\)\(/trunk\)\?$', '\1', '') . '], [0.0])') |
                    \ $put ='AC_PREREQ(2.63)' |
                    \ $put ='AC_CONFIG_SRCDIR([])' |
                    \ $put ='AC_CONFIG_AUX_DIR(config)' |
                    \ $put ='AM_INIT_AUTOMAKE(1.10)' |
                    \ $put ='' |
                    \ $put ='dnl check for required programs' |
                    \ $put ='AC_PROG_CXX' |
                    \ $put ='AC_PROG_INSTALL' |
                    \ $put ='AC_PROG_LN_S' |
                    \ $put ='AC_PROG_RANLIB' |
                    \ $put ='AC_PROG_MAKE_SET' |
                    \ $put ='' |
                    \ $put ='dnl output' |
                    \ $put ='AM_CONFIG_HEADER(config.h)' |
                    \ $put ='AC_OUTPUT(' |
                    \ $put ='	Makefile' |
                    \ $put ='	src/Makefile' |
                    \ $put ='	)' |
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
                    \     $put ='AUTOMAKE_OPTIONS = foreign dist-bzip2' |
                    \     $put ='EXTRA_DIST = autogen.bash' |
                    \ else |
                    \     $put ='MAINTAINERCLEANFILES = Makefile.in' |
                    \ endif

    augroup END
endif

"-----------------------------------------------------------------------
" mappings
"-----------------------------------------------------------------------

nmap   <silent> <S-Right>  :bnext<CR>

" v_K is really really annoying
vmap K k

" Delete a buffer but keep layout
if has("eval")
    command! Kwbd enew|bw #
    nmap     <C-w>!   :Kwbd<CR>
endif

" quickfix things
nmap <Leader>cwc :cclose<CR>
nmap <Leader>cwo :botright copen 5<CR><C-w>p
nmap <Leader>cn  :cnext<CR>

" Annoying default mappings
inoremap <S-Up>   <C-o>gk
inoremap <S-Down> <C-o>gj
noremap  <S-Up>   gk
noremap  <S-Down> gj

" Make <space> in normal mode go down a page rather than left a
" character
noremap <space> <C-f>
noremap <backspace> <C-b>

" Useful things from inside imode
inoremap <C-z>w <C-o>:w<CR>
inoremap <C-z>q <C-o>gq}<C-o>k<C-o>$

" Commonly used commands
"nmap <silent> <F3> :silent nohlsearch<CR>
"imap <silent> <F3> <C-o>:silent nohlsearch<CR>
"nmap <F4> :Kwbd<CR>
"nmap <F5> <C-w>c
"nmap <F7> :make check<CR>
"nmap <F8> :make<CR>
"nmap <F10> :!svn update<CR>
"nmap <F11> :!svn status \| grep -v '^?' \| sort -k2<CR>

" Insert a single char
noremap <Leader>i i<Space><Esc>r

" Split the line
nmap <Leader>n \i<CR>

" Pull the following line to the cursor position
noremap <Leader>J :s/\%#\(.*\)\n\(.*\)/\2\1<CR>

" In normal mode, jj escapes
inoremap jj <Esc>

" Kill line
noremap <C-k> "_dd

" Select everything
noremap <Leader>gg ggVG

" Reformat everything
noremap <Leader>gq gggqG

" Reformat paragraph
noremap <Leader>gp gqap

" Clear lines
noremap <Leader>clr :s/^.*$//<CR>:nohls<CR>

" Delete blank lines
noremap <Leader>dbl :g/^$/d<CR>:nohls<CR>

" Enclose each selected line with markers
noremap <Leader>enc :<C-w>execute
            \ substitute(":'<,'>s/^.*/#&#/ \| :nohls", "#", input(">"), "g")<CR>

" Enable fancy % matching
if has("eval")
    runtime! macros/matchit.vim
endif

" q: sucks
nmap q: :q

" set up some more useful digraphs
if has("digraphs")
    digraph ., 8230    " ellipsis (…)
endif

if has("eval")
    " GNU format changelog entry
    fun! MakeChangeLogEntry()
        norm gg
        /^\d
        norm 2O
        norm k
        call setline(line("."), strftime("%Y-%m-%d") .
                    \ " J. Alberto Suárez López <bass@gentoo.org>")
        norm 2o
        call setline(line("."), "\t* ")
        norm $
    endfun
    noremap <Leader>cl :call MakeChangeLogEntry()<CR>

    " command aliases, can't call these until after cmdalias.vim is loaded
    au VimEnter * if exists("loaded_cmdalias") |
                \       call CmdAlias("mkdir",   "!mkdir") |
                \       call CmdAlias("cvs",     "!cvs") |
                \       call CmdAlias("svn",     "!svn") |
                \       call CmdAlias("commit",  "!svn commit -m \"") |
                \       call CmdAlias("upload",  "make upload") |
                \ endif
endif

" super i_c-y / i_c-e
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

" tab completion
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

"-----------------------------------------------------------------------
" abbreviations
"-----------------------------------------------------------------------

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

"-----------------------------------------------------------------------
" special less.sh and man modes
"-----------------------------------------------------------------------

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

"-----------------------------------------------------------------------
" plugin / script / app settings
"-----------------------------------------------------------------------

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

    " clojure options
    let g:clj_want_gorilla = 1
    let g:clj_highlight_builtins = 1
    let g:clj_highlight_contrib = 1
    let g:clj_paren_rainbow = 1

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
    let Tlist_File_Fold_Auto_Close = 1
    "nnoremap <silent> <F9> :Tlist<CR>

    " Settings minibufexpl.vim
    let g:miniBufExplModSelTarget = 1
    let g:miniBufExplWinFixHeight = 1
    let g:miniBufExplWinMaxSize = 1
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

"-----------------------------------------------------------------------
" final commands
"-----------------------------------------------------------------------
" mio
let Tlist_Ctags_Cmd="/usr/bin/exuberant-ctags"
" plegado ident para python
au FileType python set foldmethod=indent
" plegado syntax para sgml,htmls,xml y xsl
au Filetype html,xml,xsl,sgml ",docbook
" explorador vertical
let g:explVertical=1
" define leader como =
let mapleader = "="

" Terminal companability
map <F15> <S-F3>
nmap <Esc>[14~ <S-F4>

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
map <F3> :Sexplore /home/hunner/<CR>
map <S-F3> :2split ~/.vim/fun_ref.vim<CR>
map <F4> :set nu<CR>
map <S-F4> :set nu!<CR>
map <F5> ggVGg?
map <F6> :set encoding=utf-8<CR> | :set fenc=utf-8<CR>
map <S-F6> :set encoding=iso8859-15<CR> | :set fenc=iso8859-15<CR>
map <F7> :SpellProposeAlternatives<CR>
map <S-F7> :SpellCheck<CR>
map <C-F7> :let spell_language_list = "english,spanish"
nnoremap <silent> <F8> :Tlist<CR>
nnoremap <silent> <S-F8> :TlistSync<CR>
nnoremap <esc> :noh<return><esc>
map <F11> !!date<CR>
map <F12> :TC<CR>
nmap  :X        :x
nmap  :W        :w
nmap  :Q        :q
noremap <Leader>rg :color relaxedgreen<CR>
noremap <Leader>ip :color inkpot<CR>
noremap <F12> <Esc>:syntax sync fromstart<CR>
inoremap <F12> <C-o>:syntax sync fromstart<CR>
syntax sync minlines=200

" Javac
set makeprg=javac\ %
set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#
map <F8> :make<CR>

" CVS
nmap <leader>cadd <Plug>CVSAdd
nmap <leader>cci <Plug>CVSCommit
nmap <leader>clog <Plug>CVSLog
map <leader>cre <Plug>CVSRevert
nmap <leader>cup <Plug>CVSUpdate
nmap <leader>cdiff <Plug>CVSDiff

" Spell
let spell_executable = "aspell"
let spell_language_list = "spanish,english"

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

"ii irc stuff
" map c1 :.w! >> ~/irc/irc.cat.pdx.edu/in<cr>dd
" map c2 :.w! >> ~/irc/irc.cat.pdx.edu/\#hack/in<cr>dd
" map c3 :.w! >> ~/irc/irc.cat.pdx.edu/\#meow/in<cr>dd
" map c4 :.w! >> ~/irc/irc.cat.pdx.edu/\#rtttoee/in<cr>dd
" map c5 :.w! >> ~/irc/irc.cat.pdx.edu/\#robots/in<cr>dd
" map c17 :.w! >> ~/irc/irc.cat.pdx.edu/\#cschat/in<cr>dd
" imap /me <C-V><C-A>ACTION

"-----------------------------------------------------------------------
" vim: set shiftwidth=4 softtabstop=4 expandtab tw=72                  :
