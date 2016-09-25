set nocompatible

" Set up Plug if not present.
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall | source ~/.vimrc
endif

call plug#begin('~/.vim/plugged')
  
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'
Plug 'flazz/vim-colorschemes'
Plug 'freeo/vim-kalisi'
" Plug 'tpope/vim-fugitive'
Plug 'ctrlpvim/ctrlp.vim', { 'on':  'CtrlP' }
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Lokaltog/vim-easymotion'
Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
"Plug 'Shougo/neosnippet'
"Plug 'Shougo/neosnippet-snippets'
"Plug 'Valloric/YouCompleteMe'
"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'jaxbot/semantic-highlight.vim', { 'on': 'SemanticHighlight' }
Plug 'edkolev/tmuxline.vim'
"Plug 'edkolev/promptline.vim'        
Plug 'myusuf3/numbers.vim'

call plug#end()

syntax enable

set autoindent          " Indentation level for next line.
set autoread            " reload files when changed.
set showmode            " Don't show mode, for airline.
set nowrap              " Don't wrap lines.
set incsearch           " Incremental search.
set ignorecase          " Ignore case while searching.
set number              " Show line numbers.
set cursorline          " Highlight current line.
set ruler               " Show where we are.
set showmatch           " Show matching brackets.
set colorcolumn=80      " Guideline at 80th column.
set shiftwidth=4        " 4 indents for tabs.
set expandtab           " Expand tabs to spaces.
set tabstop=4           " 4 column indents.
set softtabstop=4       " Insert mode tab and backspace use 4 spaces.
set clipboard=unnamed   " Yank and paste with system clipboard.
set laststatus=2        " Show status line.
set wildmenu

"set termguicolors	" Doesn't work.
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
color solarized

if has('gui_running')
    set lines=40
    set macligatures
    set guifont=Input:h13
endif

if has('statusline')
    let g:airline#extensions#tabline#enabled=1
    let g:airline#extensions#tabline#fnamemod=':t'
    let g:airline_powerline_fonts=1
endif

ino jk <esc>
cno jk <esc>
vno v <esc>
nnoremap <C-s> :w<cr>
nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>

