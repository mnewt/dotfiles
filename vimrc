" Automatically install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
  Plug 'tpope/vim-sensible'
  Plug 'ajh17/VimCompletesMe'
  Plug 'tpope/vim-vinegar'
  Plug 'itchyny/lightline.vim'
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' }
  Plug 'tpope/vim-fugitive'
  Plug 'kablamo/vim-git-log'
  Plug 'tpope/vim-surround'
  Plug 'scrooloose/nerdcommenter'
  Plug 'dag/vim-fish'
  Plug 'PProvost/vim-ps1'
  Plug 'bhurlow/vim-parinfer', { 'for': 'clojure' }
  Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
call plug#end()

set hidden

autocmd VimEnter * nnoremap <C-S-P> :CtrlPMixed<CR>
autocmd VimEnter * nnoremap <leader>b :CtrlPBuffer<CR>

" netrw settings
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
