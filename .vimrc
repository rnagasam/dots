set nocompatible

execute pathogen#infect()

syntax enable
filetype plugin indent on

nnoremap j gj
nnoremap k gk

set laststatus=2

set path+=**
set wildmenu

let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3

set noerrorbells
set novisualbell
set t_vb=
set tm=500

set backspace=indent,eol,start

set autoindent
set expandtab
set smarttab
set wrap

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

highlight OverLength ctermbg=red ctermfg=white guibg=#592929

set ruler

set history=999

let mapleader=","

set autoread

set ignorecase
set smartcase
set hlsearch
set incsearch

set lazyredraw

set magic

set showmatch

nnoremap <leader><leader> :nohl<CR>

set ffs=unix,dos,mac

set nobackup
set nowb
set noswapfile

" Managing splits
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove<cr>
map <leader>tt :tabnext<cr>
map <leader>tp :-tabnext<cr>

" Managing buffers
map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>
map <leader>b :ls<CR>:b
