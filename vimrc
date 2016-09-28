call plug#begin('~/.vim/plugged')
  Plug 'tpope/vim-sensible'
  Plug 'itchyny/lightline.vim'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'sjbach/lusty'
  Plug 'tpope/vim-fugitive'
  Plug 'kablamo/vim-git-log'
  Plug 'dag/vim-fish'
  Plug 'PProvost/vim-ps1'
  Plug 'oakmac/parinfer-viml', { 'for': 'clojure' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
call plug#end()

set hidden

autocmd VimEnter * nnoremap <C-S-P> :CtrlPMixed<CR>
autocmd VimEnter * nnoremap <leader>b :CtrlPBuffer<CR>
autocmd VimEnter * nnoremap <leader>/ :LustyBufferGrep<CR>

