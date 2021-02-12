" vim-numtoggle config
:set number relativenumber

" vim-airline config
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = 'dark'
let g:airline_left_sep = '⮀'

" limelight.vim config
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

" NERDTree config
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>
