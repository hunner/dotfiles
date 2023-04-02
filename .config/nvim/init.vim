call plug#begin()
" For git shortcuts below.
Plug 'tpope/vim-fugitive'
" To make :Gbrowse work. Shortcut below.
Plug 'tpope/vim-rhubarb'

" Auto linting! See triggers below, plus built-in ones at https://github.com/neomake/neomake/blob/master/autoload/neomake/makers/ft/ruby.vim
Plug 'neomake/neomake'

" General completion. Needs plugins to extend
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp' " for ncm2 -- seems to cause network timeouts sometimes
" NOTE: you need to install completion sources to get completions. Check
" our wiki page for a list of sources: https://github.com/ncm2/ncm2/wiki
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-path'

" Language-aware fancy things. Needs lang server config. Used, but what for?
" Migrating to nvim-lspconfig
"Plug 'autozimu/LanguageClient-neovim', {
"      \ 'branch': 'next',
"      \ 'do': 'bash install.sh',
"      \ }
Plug 'neovim/nvim-lspconfig'

" Fancy tab completion; did I ever use it? I think it needs other plugins
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Menu browser... I like fzf more for files; this is for neoyank
Plug 'Shougo/denite.nvim' | Plug 'Shougo/neoyank.vim'

" Snippets... I don't use them
"Plug 'Shougo/neosnippet.vim' | Plug 'Shougo/neosnippet-snippets'

" Show function signature in the command line (see noshowmode below)
Plug 'Shougo/echodoc.vim'

" Undo tree mapped below
Plug 'simnalamburt/vim-mundo'

" Netrw is nice, but so is this
" Edit: this slows nvim's start-up time way down
Plug 'scrooloose/nerdtree'

" Motions... need to actually document them otherwise I forget
Plug 'easymotion/vim-easymotion'

" Align. See mapping below
"Plug 'vim-scripts/Align'
Plug 'junegunn/vim-easy-align'

" Opening files/buffers. Mapped below.
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Also adds :Commits and :BCommits
Plug 'junegunn/fzf.vim'

" Reopen at the last place
Plug 'dietsche/vim-lastplace'

" Nice status line
Plug 'itchyny/lightline.vim'

" Make tmux status line match
Plug 'edkolev/tmuxline.vim'

" Color schemes
Plug 'jacoborus/tender.vim'
Plug 'ciaranm/inkpot'
Plug 'arcticicestudio/nord-vim'
Plug 'twerth/ir_black'

" Generate UUIDs
Plug 'kburdett/vim-nuuid'

" Format js prettier
Plug 'sbdchd/neoformat'

" Various langs
Plug 'rodjek/vim-puppet'
Plug 'keith/swift.vim'
Plug 'darfink/vim-plist'
Plug 'vim-ruby/vim-ruby'
Plug 'thoughtbot/vim-rspec'
Plug 'hashivim/vim-terraform'
Plug 'vim-syntastic/syntastic'
Plug 'juliosueiras/vim-terraform-completion'
Plug 'chrisbra/csv.vim'
Plug 'wlangstroth/vim-racket'
Plug 'kchmck/vim-coffee-script'
Plug 'jceb/vim-orgmode'
Plug 'bfontaine/Brewfile.vim'
Plug 'pprovost/vim-ps1'
" This needs nodejs 14.14 or greater
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'mdempsky/gocode', { 'rtp': 'nvim', 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh' }
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" For vim-orgmode
Plug 'tpope/vim-speeddating'
Plug 'zpieslak/vim-autofix'
Plug 'juvenn/mustache.vim'
Plug 'github/copilot.vim'
Plug 'hunner/vim-beancount'
call plug#end()

" Because.
noremap <Space> :
let mapleader = ","

nnoremap <C-u> :MundoToggle<CR>
" <Leader><Leader>+(s)earch, (w)ord for jumping

noremap <Space>fs :w<CR>
"noremap <Space>qq :q<CR>
"noremap <Space>qa :qa<CR>
"noremap <Space>ff :Files<CR>
"noremap <Space>pf :GFiles<CR>
"noremap <Space>bb :Buffers<CR>

" From fzf.vim
noremap <Leader>f :Files<CR>
noremap <Leader>F :Files %:p:h<CR>
noremap <Leader>v :GFiles<CR>
" TODO I'd like to merge the history / buffers list
noremap <Leader>b :Buffers<CR>
noremap <Leader>h :History<CR>
noremap <Leader>gc :Commits<CR>
noremap <Leader>gb :BCommits<CR>
noremap <Leader>c :ChangeDir<CR>
"noremap <Leader>u :FufRenewCache<CR>
"noremap <Leader>w :bdelete<CR>
noremap <F1> :Helptags<CR>

" To close the current buffer without closing the current window
noremap <Leader>q :bp<bar>sp<bar>bn<bar>bd<CR>

" From vim-rhubarb
noremap <Leader>gh :GBrowse<CR>

" Edit a file in the same directory as the current buffer
noremap <Leader>e :e %:p:h/

let g:beancount_separator_col = 60
nnoremap <buffer> <leader>= :AlignCommodity<CR>
vnoremap <buffer> <leader>= :AlignCommodity<CR>

" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --color: Search color options
" --glob: Additional conditions for search, ignore with ! prefix
let g:rg_command = '
  \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
  \ --glob "!{.git,node_modules,vendor}/*" '
command! -bang -nargs=* Rg call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)
noremap <Leader>/ :Rg<CR>
"noremap <Space>/ :Rg<CR>

function! s:get_buffer_git_root(...)
  let root = fugitive#repo().tree(expand('%:p:h'))
  if empty(root)
    return s:warn('Not in git repo')
  endif
  return fzf#run('bfiles', {
  \ 'source':  'git ls-files',
  \ 'dir':     root,
  \ 'options': '-m --prompt "GitFiles> "'
  \}, a:000)
endfunction
command! -bar -bang BFiles
  \ call s:get_buffer_git_root(<bang>0)

function! s:changedir(...)
  return s:fzf('changedir', {
  \ 'source':  'find ~ -type d -print',
  \ 'options': '-m --prompt "Dir> "'
  \}, a:000)
endfunction
command! -bar -bang ChangeDir
  \ call fzf#run(fzf#wrap('changedir', {
  \ 'dir': '~',
  \ 'source': 'find . -type d',
  \ 'sink': 'cd'
  \ }, <bang>0))

" 24-bit blue/orange theme
colorscheme nord
scriptencoding utf-8
set encoding=utf-8
let g:lightline = {
  \ 'colorscheme': 'nord',
  \ 'component': {
  \   'lineinfo': '0x%02B:%03l:%-2v'
  \ },
  \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
  \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
  \ }
" Make tmux match. Uses tmuxline.vim
let g:tmuxline_powerline_separators = 1 " yes please
let g:tmuxline_preset = 'full' " info on the line
"autocmd InsertEnter * :v
"autocmd VisualEnter * call :Tmu

" Go lang settings
let g:go_highlight_array_whitespace_error = 1
let g:go_highlight_chan_whitespace_error = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_parameters = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_build_constraints = 1
let g:go_highlight_generate_tags = 1
let g:go_highlight_string_spellcheck = 1
let g:go_highlight_format_strings = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_variable_assignments = 1
let g:go_auto_type_info = 1
let g:go_updatetime = 250
let g:go_build_tags = 'wireinject' " include wire.go but not wire_gen.go in gopls scope
au FileType go nmap <Leader>gor <Plug>(go-run)
au FileType go nmap <Leader>gob <Plug>(go-build)
au FileType go nmap <Leader>got <Plug>(go-test)
au FileType go nmap <Leader>goc <Plug>(go-coverage)
au FileType go nmap <C-M-b> :GoImplements<CR>
au FileType go nmap <C-M-Right> :GoDef<CR>
au FileType go nmap <C-M-Left> :GoDefPop<CR>

" Neoformat search local node_modules prettier
let g:neoformat_try_node_exe = 1
" Use formatprg if available
let g:neoformat_try_formatprg = 1
autocmd FileType javascript setlocal formatprg=prettier\ --single-quote\ --trailing-comma\ es5
" autocmd BufWritePre *.js Neoformat
autocmd BufWritePre *.jsx Neoformat

" Disable nerdtree because it's way slow and I don't use it. fzf4evr
nnoremap <silent> <C-G> :NERDTreeToggle<CR>
function! ToggleNerdTreeFind()
  if(exists("b:NERDTree") && b:NERDTree.IsOpen())
    exec(':NERDTreeClose')
  else
    exec(':NERDTreeFind')
  endif
endfunction
nnoremap <silent> <C-g> :call ToggleNerdTreeFind()<CR>

" nerdtree alternative. I didn't finish this. https://shapeshed.com/vim-netrw/
"let g:netrw_banner = 0
"let g:netrw_liststyle = 3
"let g:netrw_browse_split = 1
"let g:netrw_winsize = 15
"augroup ProjectDrawer
"  autocmd!
"  "autocmd VimEnter * :Vexplore
"augroup END

noremap <Leader>tn :color tender<CR>
noremap <Leader>ip :color inkpot<CR>
noremap <Leader>ir :color ir_black<CR>
noremap <Leader>fed :e ~/.config/nvim/init.vim<CR>
noremap <Leader>tod :e ~/Dropbox/todo.txt<CR>

" TODO quickfix for ripgrep results
" TODO quickfix for autolint and rake stuff
autocmd QuickFixCmdPost * botright cwindow 6
" quickfix things like '-'
"nmap <Leader>cwc :cclose<CR>
"nmap <Leader>cwo :botright copen 5<CR><C-w>p
"nmap <Leader>cn  :cnext<CR>
"nmap <Leader>cp  :cprevious<CR>
"nmap -  :cnext<CR>
"nmap _  :cprev<CR>
"nmap <C--> :colder<CR>
"nmap <C-_> :cnewer<CR>
" neomake uses the location window rather than quickfix window
noremap -  :lnext<CR>
noremap _  :lprev<CR>
" See ToggleLocationList below

" Only lint when leaving insert or changing text in command mode
call neomake#configure#automake({
  \ 'InsertLeave': {'delay': 0},
  \ 'TextChanged': {},
  \ }, 500)
let g:neomake_python_enabled_makers = ['pylint']
let g:neomake_ruby_rubocop_maker = {
  \ 'args': ['exec', 'rubocop', '--format', 'emacs', '--force-exclusion', '--display-cop-names'],
  \ 'exe': 'bundle',
  \ 'errorformat': '%f:%l:%c: %t: %m,%E%f:%l: %m',
  \ 'postprocess': function('neomake#makers#ft#ruby#RubocopEntryProcess'),
  \ 'output_stream': 'stdout',
  \ }
let g:neomake_ruby_rubocop_rails_maker = {
  \ }

" RSpec.vim mappings
map <Leader>rf :call RunCurrentSpecFile()<CR>
map <Leader>rt :call RunNearestSpec()<CR>
map <Leader>rl :call RunLastSpec()<CR>
map <Leader>ra :call RunAllSpecs()<CR>

let g:deoplete#enable_at_startup = 1

" ncm2 commands for completion
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()
" :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Show echodoc instead of -- INSERT -- in the status line.
set noshowmode

" Undo files in undodir=~/.local/share/nvim/undo/
" Backup files in backupdir=~/.local/share/nvim/backup/
set undofile

" Allow unwritten buffers to be hidden
set hidden

" Enable live preview of replace
set inccommand=split

" Copy between instances
let g:neoyank#file = $HOME.'/.local/share/nvim/yankring.txt'
nmap <C-p> :Denite neoyank<CR>
"vmap <Leader>y :'<,'>! cat \| tee ~/.local/share/nvim/yank.txt<CR>
"nmap <Leader>p o<Esc>:.!cat ~/.local/share/nvim/yank.txt<CR>


function! GetBufferList()
  redir =>buflist
  silent! ls!
  redir END
  return buflist
endfunction

function! ToggleLocationList()
  let buflist = GetBufferList()
  " Close if it's open
  for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "Location List"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
    if bufwinnr(bufnum) != -1
      exec('lclose')
      return
    endif
  endfor
  " Don't open if it's empty
  if len(getloclist(0)) == 0
    echohl ErrorMsg
    echo "Location List is Empty."
    return
  endif
  " Open it
  let winnr = winnr()
  exec('lopen')
  " Go back to window
  if winnr() != winnr
    wincmd p
  endif
endfunction
noremap <Leader>- :call ToggleLocationList()<CR>

" ^n Show number and fold columns in windows
function! <SID>FoldNumbers()
  " If we're in a wide window, enable line numbers.
  "if winwidth(0) >= 76 " 72 + 4, or should I use tw?
    " Add folds, or cycle through number schemes
    if &foldlevel < 99 && &foldenable && &foldcolumn == 0
      setlocal foldcolumn=1
    elseif (&foldlevel == 99 || ! &foldenable) && &foldcolumn != 0
      setlocal foldcolumn=0
    elseif ! &rnu && ! &nu
      setlocal relativenumber
    elseif &rnu
      setlocal number
      setlocal norelativenumber
    elseif &nu
      setlocal nonumber
    endif
  "else
  "  setlocal norelativenumber
  "  setlocal nonumber
  "  setlocal foldcolumn=0
  "endif
endfun
"autocmd WinEnter,BufWinEnter,BufNew * :call <SID>FoldNumbers()
noremap <silent> <C-n> :call <SID>FoldNumbers()<CR>

" Case insensitivity for searching
set ignorecase
set infercase

" Indentation
set expandtab
set tabstop=2
set shiftwidth=2
set smartindent

" More sane directory completion
set wildmode=longest,list,full
set wildignorecase

" C-c and <Esc> are not entirely the same, but I want them to be. Also C-g
" because emacs habits.
inoremap <C-c> <Esc>
inoremap <C-g> <Esc>

" Scrolling with arrows controls the window
noremap <Up>   <C-y>
noremap <Down> <C-e>

" Movement between windows with ^hjkl
nmap <BS>  <C-w>h
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" Insert pry line
noremap <Leader>pry orequire'pry';binding.pry<Esc>
noremap <Leader>PRY Orequire'pry';binding.pry<Esc>

" Clear search highlights
nnoremap <Return> :noh<CR>

" Show tabs and trailing whitespace visually {{{2
set list listchars=tab:»·,trail:·,extends:…,nbsp:‗

" Languageserver settings from the solargraph readme
" Tell the language client to use the default IP and port
" that Solargraph runs on
"let g:LanguageClient_serverCommands = {
"      \ 'go': ['gopls'],
"      \ 'ruby': ['~/.rbenv/shims/solargraph', 'stdio'],
"      \ 'python': ['pyls'],
"      \ }

" note that if you are using Plug mapping you should not use `noremap` mappings.
"nmap <F5> <Plug>(lcn-menu)

" Delete a buffer but keep layout with ^w!
if has("eval")
  command! Kwbd enew|bw #
  nmap     <C-w>!   :Kwbd<CR>
endif

" Don't send a stop signal to the server when exiting vim.
" This is optional, but I don't like having to restart Solargraph
" every time I restart vim.
"let g:LanguageClient_autoStop = 0

" Configure ruby omni-completion to use the language client:
"autocmd FileType ruby setlocal omnifunc=LanguageClient#complete

" Run gofmt on save
"autocmd BufWritePre *.go :call LanguageClient#textDocument_formatting_sync()


" {{{ Adding some terraform stuff
" Syntastic Config
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" (Optional)Remove Info(Preview) window
set completeopt-=preview

" (Optional)Hide Info(Preview) window after completions
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" (Optional) Enable terraform plan to be include in filter
let g:syntastic_terraform_tffilter_plan = 1

" (Optional) Default: 0, enable(1)/disable(0) plugin's keymapping
let g:terraform_completion_keys = 1

" (Optional) Default: 1, enable(1)/disable(0) terraform module registry completion
let g:terraform_registry_module_completion = 0

"let g:deoplete#omni_patterns = {}
"let g:deoplete#omni_patterns.terraform = '[^ *\t"{=$]\w*'
"let g:deoplete#enable_at_startup = 1
"call deoplete#initialize()
" }}} end terraform
source ~/.config/nvim/tf.lua

" CoC stuff
set updatetime=300 " faster than 4000ms
function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
let g:copilot_no_tab_map = v:true
imap <silent><script><expr> <C-e> coc#pum#visible() ? coc#pum#cancel() : copilot#Accept("\<C-e>")
inoremap <expr> <cr> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
inoremap <silent><expr> <c-space> coc#refresh()
" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>
