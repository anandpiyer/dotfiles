" Vundle {
    set nocompatible
    filetype on
    filetype off

    if has('win32') || has('win64')
        set rtp+=~/vimfiles/bundle/vundle/
        call vundle#rc('$HOME/vimfiles/bundle/')
    else
        set rtp+=~/.vim/bundle/vundle/
        call vundle#rc()
    endif

    " Bundles {
        Bundle 'gmarik/vundle'
        Bundle 'bling/vim-airline'
        Bundle 'altercation/vim-colors-solarized'
        Bundle 'flazz/vim-colorschemes'
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

" }

" General {
    
    set background=dark         " Assume dark background.
    syntax on                   " Syntax highlight.
    set spell                   " Spell checker.

" }

" Vim UI {

    let color_file="~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"
    if has('win32') || has('win64')
        let color_file="~/vimfiles/bundle/vim-colors-solarized/colors/solarized.vim"
    endif

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
        if has('win32') || has('win64')
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
