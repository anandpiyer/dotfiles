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

" Plug {
"
    " Set up Plug if not present.
    if empty(glob('~/.vim/autoload/plug.vim'))
        silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall | source ~/.vimrc
    endif

    set encoding=utf-8
    set nocompatible

    call plug#begin('~/.vim/plugged')

    " Bundles {
  
        " Status line
        Plug 'vim-airline/vim-airline'
        Plug 'vim-airline/vim-airline-themes'

        " Color scheme/themes
        Plug 'altercation/vim-colors-solarized'
        Plug 'flazz/vim-colorschemes'
       
        " Git
        " Plug 'tpope/vim-fugitive'

        " Fuzzy search
        Plug 'ctrlpvim/ctrlp.vim', { 'on':  'CtrlP' }

        " File search
        Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

        Plug 'Lokaltog/vim-easymotion'

        " Latex
        Plug 'LaTeX-Box-Team/LaTeX-Box'
        "Plug 'lervag/vimtex'        

        " Scala
        Plug 'derekwyatt/vim-scala', { 'for': 'scala' }

        Plug 'nathanaelkane/vim-indent-guides'

        "Plug 'Shougo/neocomplete.vim.git'
        "Plug 'Shougo/neosnippet'
        "Plug 'Shougo/neosnippet-snippets'
        "Plug 'honza/vim-snippets'

        "Plug 'Valloric/YouCompleteMe'
        Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

        Plug 'jaxbot/semantic-highlight.vim', { 'on': 'SemanticHighlight' }
    " }

    call plug#end()

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
    set colorcolumn=80          " Guideline at 80th column.
" }

" General {
    
    set background=dark         " Assume dark background.
    syntax on                   " Syntax highlight.
    set spell                   " Spell checker.

" }

" Vim UI {

    let color_file="~/.vim/bundle/vim-colors-solarized/colors/solarized.vim"

    if filereadable(expand(color_file))
        if has('gui_running')
            let g:solarized_termcolors=256
        endif
        let g:solarized_termtrans=1
        let g:solarized_contrast="normal"
        let g:solarized_visibility="normal"
        color solarized
    endif

    if has('statusline')
        set laststatus=2
        "set statusline=%<%f\                     " Filename
        "set statusline+=%w%h%m%r                 " Options
        ""set statusline+=%{:fugitive#statusline()} " Git Hotness
        "set statusline+=\ [%{&ff}/%Y]            " Filetype
        "set statusline+=\ [%{getcwd()}]          " Current dir
        "set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
        let g:airline#extensions#tabline#enabled = 1
        let g:airline_powerline_fonts=1
        "let g:Powerline_symbols='unicode'
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
            set guifont=Monaco\ for\ Powerline:h12
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

" Key remaps { 
    nnoremap <C-s> :w<cr>
    nnoremap <Tab> :bnext<CR>
    nnoremap <S-Tab> :bprevious<CR>
" }

" LaTeX {
    augroup Formatting
        autocmd!
        autocmd BufNew,BufRead *.tex setlocal formatoptions=tcnr textwidth=80 wrapmargin=0
    augroup END
    "augroup PROSE 
    "autocmd InsertEnter * set formatoptions+=a 
    "autocmd InsertLeave * set formatoptions-=a 
    "augroup END
    
    " Highlight all characters after 80th.
    "augroup vimrc_autocmds
    "   autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
    "   autocmd BufEnter * match OverLength /\%80v.*/
    "augroup END

    let g:tex_flavor = "latex"

    let g:latex_latexmk_continuous=0
    let g:latex_latexmk_background=1
    
    let g:LatexBox_quickfix=2
" }

" NerdTree {
    if isdirectory(expand("~/.vim/bundle/nerdtree"))
        set autochdir
        map <C-e> <plug>NERDTreeTabsToggle<CR>
        map <leader>e :NERDTreeFind<CR>
        nmap <leader>nt :NERDTreeFind<CR>

        let NERDTreeShowBookmarks=1
        let NERDTreeIgnore=['\.py[cd]$', '\~$', '\.swo$', '\.swp$', '^\.git$', '^\.hg$', '^\.svn$', '\.bzr$']
        let NERDTreeChDirMode=2
        let NERDTreeQuitOnOpen=1
        let NERDTreeMouseMode=2
        let NERDTreeShowHidden=1
        let NERDTreeKeepTreeInNewTab=1
        let g:nerdtree_tabs_open_on_gui_startup=0
    endif
" }
"
" ctrlp {
    if isdirectory(expand("~/.vim/plugged/ctrlp.vim/"))
        let g:ctrlp_working_path_mode = 'ra'
        nnoremap <silent> <C-p> :CtrlP<CR>
        nnoremap <silent> <C-b> :CtrlPBuffer<CR>
        let g:ctrlp_custom_ignore = {
            \ 'dir':  '\.git$\|\.hg$\|\.svn$',
            \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

        " On Windows use "dir" as fallback command.
        if WINDOWS()
            let s:ctrlp_fallback = 'dir %s /-n /b /s /a-d'
        elseif executable('ag')
            let s:ctrlp_fallback = 'ag %s --nocolor -l -g ""'
        elseif executable('ack-grep')
            let s:ctrlp_fallback = 'ack-grep %s --nocolor -f'
        elseif executable('ack')
            let s:ctrlp_fallback = 'ack %s --nocolor -f'
        else
            let s:ctrlp_fallback = 'find %s -type f'
        endif
        let g:ctrlp_user_command = {
            \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
            \ },
            \ 'fallback': s:ctrlp_fallback
        \ }
    endif
"}
