call plug#begin('~/.vim/plugged')
  Plug 'tpope/vim-sensible'
  Plug 'itchyny/lightline.vim'
  Plug 'tpope/vim-fugitive'
  Plug 'kablamo/vim-git-log'
  Plug 'dag/vim-fish'
  Plug 'PProvost/vim-ps1'
  Plug 'oakmac/parinfer-viml', { 'for': 'clojure' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
  Plug 'tpope/vim-vividchalk'
call plug#end()

colorscheme vividchalk
