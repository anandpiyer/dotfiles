" Platform {
    silent function! WINDOWS()
        return (has('win32') || has('win64'))
    endfunction
    silent function! OSX()
        return has('macunix')
    endfunction
    silent function! LINUX()
        return has('unix') && !has('macunix') && !has('win32unix')
    endfunction

    if WINDOWS()
        set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME
    endif
" }

" Vundle {
"
    set nocompatible
    filetype on
    filetype off

    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    " Bundles {
  
        Bundle 'gmarik/vundle'
        
        " Status line
        Bundle 'bling/vim-airline'
        
        " Color scheme/themes
        Bundle 'altercation/vim-colors-solarized'
        Bundle 'flazz/vim-colorschemes'
        
        " Fuzzy search
        Bundle 'kien/ctrlp.vim'

        " File search
        Bundle 'scrooloose/nerdtree'

        Bundle 'Lokaltog/vim-easymotion'

        " Latex
        Bundle 'LaTeX-Box-Team/LaTeX-Box'
    
    " }

    filetype plugin indent on
    " Brief help
    " :BundleList          - list configured bundles
    " :BundleInstall(!)    - install (update) bundles
    " :BundleSearch(!) foo - search (or refresh cache first) for foo
    " :BundleClean(!)      - confirm (or auto-approve) removal of unused bundles

    " see :h vundle for more details or wiki for FAQ
    " NOTE: comments after Bundle commands are not allowed.
 "}

" Formatting {

    set showmode                " Don't show mode, airline shows it.
    set nowrap                  " No wrapping of lines.
    set autoindent              " Same indentation level as previous line.
    set shiftwidth=4            " 4 indents for tab
    set expandtab               " Use space instead of tabs
    set tabstop=4               " 4 column indents.
    set softtabstop=4

" }

" General {
    
    set background=dark         " Assume dark background.
    syntax on                   " Syntax highlight.
    set spell                   " Spell checker.

" }

" Vim UI {

    let color_file="~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"

    if filereadable(expand(color_file))
        let g:solarized_termcolors=256
        let g:solarized_termtrans=1
        let g:solarized_contrast="normal"
        let g:solarized_visibility="normal"
        color solarized
    endif

    if has('statusline')
        set laststatus=2
        set statusline=%<%f\                     " Filename
        set statusline+=%w%h%m%r                 " Options
        set statusline+=%{fugitive#statusline()} " Git Hotness
        set statusline+=\ [%{&ff}/%Y]            " Filetype
        set statusline+=\ [%{getcwd()}]          " Current dir
        set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
    endif
    
    set nu                      " Show line numbers.
    set foldenable              " Automatic code folding.
    set cursorline              " Highlight current line.
    set showmatch               " Matching brackets.
" }

" GUI Settings {

    if has('gui_running')
        if WINDOWS()
            set lines=20
            set guifont=Consolas:h10
        else
            set lines=40
            set guifont=Monaco:h12
        endif
        if has('gui_macvim')
            set transparency=5
        endif
    else
        if &term == 'xterm' || &term == 'screen'
            set t_Co=256
        endif
    endif

"  }
