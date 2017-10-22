
call plug#begin('~/.vim/plugged')
  Plug 'tpope/vim-sensible'
  Plug 'tpope/vim-vinegar'
  Plug 'itchyny/lightline.vim'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --key-bindings --completion --no-update-rc'}
  Plug 'junegunn/fzf.vim'
  Plug 'ajh17/VimCompletesMe'
  Plug 'mbbill/undotree'
  Plug 'tpope/vim-eunuch'
  Plug 'tpope/vim-surround'
  Plug 'scrooloose/nerdcommenter'
  Plug 'tpope/vim-fugitive'
  Plug 'kablamo/vim-git-log'
  Plug 'vitalk/vim-shebang'
  Plug 'jiangmiao/auto-pairs'
  Plug 'bhurlow/vim-parinfer', { 'for': 'clojure'}
  Plug 'tpope/vim-fireplace', { 'for': 'clojure'}
  Plug 'dag/vim-fish'
  Plug 'PProvost/vim-ps1'
  Plug 'isobit/vim-caddyfile'
call plug#end()

" lightline
set noshowmode
let g:lightline = {
      \ 'active': {
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'filetype' ] ]
      \ },
      \ }

nnoremap <S-u> redo

" tabs
map <leader>1 1gt
map <leader>2 2gt
map <leader>3 3gt
map <leader>4 4gt
map <leader>5 5gt
map <leader>6 6gt
map <leader>7 7gt
map <leader>8 8gt
map <leader>9 9gt

" netrw
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25

" NERD Commenter
let g:NERDSpaceDelims = 1
map <leader>; <leader>c<Space>

" AutoPair
autocmd FileType clojure,lisp,scheme,racket let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
autocmd FileType vim let b:AutoPairs = {'(':')', '[':']', '{':'}',"'":"'", '`':'`'}

