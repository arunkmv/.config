call plug#begin()

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'junegunn/limelight.vim'
Plug 'tpope/vim-fugitive'
Plug 'preservim/nerdtree'
Plug 'r901042004/riscv.vim'
Plug 'rbgrouleff/bclose.vim'

call plug#end()

autocmd FileType json syntax match Comment +\/\/.\+$+
