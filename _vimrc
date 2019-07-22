syntax on
set guifont=Consolas
set nocompatible
set encoding=utf-8
set wrap
filetype off

set rtp+=$HOME/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'sainnhe/gruvbox-material'
Plugin 'itchyny/lightline.vim' 
call vundle#end()

autocmd VimEnter * NERDTree
autocmd BufEnter * NERDTreeMirror
autocmd VimEnter * wincmd w
filetype on
colorscheme gruvbox-material
let g:lightline = {'colorscheme' : 'gruvbox_material'}
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
set number

:let g:latex_to_unicode_auto = 1
