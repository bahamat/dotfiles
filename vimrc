""""
"""" More comfortable working environment
""""

" Tabs suck
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2

" Nicer environment
set linebreak
set ruler
set shortmess+=I
set showmode

" Additional options
set laststatus=2              " always have a status bar
set scrolloff=1               " don't let the curser get too close to the edge

" Beautify
syntax enable
set background=dark

" Solarized
"let g:solarized_termcolors=256
let g:solarized_termtrans =1
let g:solarized_degrade   =0
let g:solarized_bold      =0
let g:solarized_underline =0
let g:solarized_italic    =0
let g:solarized_contrast  ="high"
let g:solarized_visibility="high"
colorscheme solarized

""""
"""" Remap Commands
""""

" Disable keys that annoy me
nnoremap Q <nop>

" Forgot to sudo? Here's the answer.
" This nice little baby I found at http://solidstateraam.com/saving-files-as-root-from-inside-vim/
cmap w!! w !sudo tee % >/dev/null

" Encrypt/Decrypt arbitrary files
" cmap encrypt % !openssl enc -e -aes256 -salt -a
" cmap decrypt % !openssl enc -d -aes256 -salt -a

""""
"""" Automation
""""

" From http://www.debian-administration.org/articles/571
" Sets +x on stuff starting with the magic shebang.
au BufWritePost * if getline(1) =~ "^#!" | silent !chmod a+x <afile>

""""
"""" Custom File Types
""""

" for Cfengine
au BufRead,BufNewFile cf.* set ft=cf3
au BufRead,BufNewFile *.cf set ft=cf3

" for Apache
au BufRead,BufNewFile *.net set ft=apache
au BufRead,BufNewFile *.org set ft=apache
