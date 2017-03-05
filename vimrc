call plug#begin('~/.vim/plugged')
  Plug 'tpope/vim-sensible'
  Plug 'itchyny/lightline.vim'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'sjbach/lusty'
  Plug 'tpope/vim-fugitive'
  Plug 'kablamo/vim-git-log'
  Plug 'dag/vim-fish'
  Plug 'PProvost/vim-ps1'
  Plug 'bhurlow/vim-parinfer', { 'for': 'clojure' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
call plug#end()

set hidden

autocmd VimEnter * nnoremap <C-S-P> :CtrlPMixed<CR>
autocmd VimEnter * nnoremap <leader>b :CtrlPBuffer<CR>
autocmd VimEnter * nnoremap <leader>/ :LustyBufferGrep<CR>

filetype plugin indent on
" show existing tab with 2 spaces width
set tabstop=2
set softtabstop=2
" when indenting with '>', use 2 spaces width
set shiftwidth=2
" On pressing tab, insert 2 spaces
set expandtab

