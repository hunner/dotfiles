call plug#begin()
Plug 'tpope/vim-fugitive'
Plug 'simnalamburt/vim-mundo'
Plug 'easymotion/vim-easymotion'
Plug 'vim-scripts/Align'
Plug 'neomake/neomake' | Plug 'dojoteef/neomake-autolint'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
Plug 'Shougo/deoplete.nvim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'dietsche/vim-lastplace'
Plug 'itchyny/lightline.vim'
Plug 'jacoborus/tender.vim'
Plug 'ciaranm/inkpot'
Plug 'twerth/ir_black'
Plug 'rodjek/vim-puppet'
call plug#end()

" Because.
noremap <Space> :
let mapleader = ","

nnoremap <C-U> :MundoToggle<CR>
" <Leader><Leader>+(s)earch, (w)ord for jumping
noremap <Leader>f :Files<CR>
noremap <Leader>F :Files %:p:h<CR>
noremap <Leader>v :GFiles<CR>
noremap <Leader>b :Buffers<CR>
noremap <Leader>h :History<CR>
noremap <Leader>gc :Commits<CR>
noremap <Leader>gb :BCommits<CR>
noremap <Leader>c :ChangeDir<CR>
"noremap <Leader>u :FufRenewCache<CR>
"noremap <Leader>w :bdelete<CR>
noremap <Leader>/ :Ag<Space>
noremap <F1> :Helptags<CR>

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
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' }
      \ }

noremap <Leader>tn :color tender<CR>
noremap <Leader>ip :color inkpot<CR>
noremap <Leader>ir :color ir_black<CR>

" Undo files in undodir=~/.local/share/nvim/undo/
set undofile

" Case insensitivity for searching
set ignorecase
set infercase

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

" Clear search highlights with <Esc>
nnoremap <Esc> :noh<CR><Esc>

" Show tabs and trailing whitespace visually {{{2
set list listchars=tab:»·,trail:·,extends:…,nbsp:‗
