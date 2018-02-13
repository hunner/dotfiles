call plug#begin()
" Why?
Plug 'tpope/vim-fugitive'

" Auto linting! See triggers below, plus built-in ones at https://github.com/neomake/neomake/blob/master/autoload/neomake/makers/ft/ruby.vim
Plug 'neomake/neomake'

" General completion. Needs plugins to extend
Plug 'roxma/nvim-completion-manager'

" Language-aware fancy things. Needs lang server config. Used, but what for?
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }

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

" Motions... need to actually document them otherwise I forget
Plug 'easymotion/vim-easymotion'

" Align. See mapping below
"Plug 'vim-scripts/Align'
Plug 'junegunn/vim-easy-align'

" Opening files/buffers. Mapped below
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Reopen at the last place
Plug 'dietsche/vim-lastplace'

" Nice status line
Plug 'itchyny/lightline.vim'

" Color schemes
Plug 'jacoborus/tender.vim'
Plug 'ciaranm/inkpot'
Plug 'twerth/ir_black'

" Various langs
Plug 'rodjek/vim-puppet'
Plug 'keith/swift.vim'
Plug 'hunner/vim-plist'
Plug 'vim-ruby/vim-ruby'
Plug 'kchmck/vim-coffee-script'
call plug#end()

" Because.
noremap <Space> :
let mapleader = ","

nnoremap <C-u> :MundoToggle<CR>
" <Leader><Leader>+(s)earch, (w)ord for jumping

"noremap <Space>fs :w<CR>
"noremap <Space>qq :q<CR>
"noremap <Space>qa :qa<CR>
"noremap <Space>ff :Files<CR>
"noremap <Space>pf :GFiles<CR>
"noremap <Space>bb :Buffers<CR>

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
" Edit a file in the same directory as the current buffer
noremap <Leader>e :e %:p:h/

" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options
command! -bang -nargs=* Rg call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)
noremap <Leader>/ :Rg<CR>
"noremap <Space>/ :Rg<CR>

function s:get_buffer_git_root(...)
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
colorscheme tender
let g:lightline = {
  \ 'colorscheme': 'tenderplus',
  \ 'component': {
  \   'lineinfo': '0x%02B:%03l:%-2v'
  \ },
  \ 'separator': { 'left': '', 'right': '' },
  \ 'subseparator': { 'left': '', 'right': '' }
  \ }

noremap <Leader>tn :color tender<CR>
noremap <Leader>ip :color inkpot<CR>
noremap <Leader>ir :color ir_black<CR>
noremap <Leader>fed :e ~/.config/nvim/init.vim<CR>

" TODO quickfix for ripgrep results
" TODO quickfix for autolint and rake stuff

" Only lint when leaving insert or changing text in command mode
call neomake#configure#automake({
  \ 'InsertLeave': {'delay': 0},
  \ 'TextChanged': {},
  \ }, 500)

let g:deoplete#enable_at_startup = 1

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

" ^n Show number and fold columns in windows {{{2
if has("eval")
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
endif

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

" C-c and <Esc> are not entirely the same, but I want them to be
inoremap <C-c> <Esc>

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
