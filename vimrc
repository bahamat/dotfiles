"   Copyright 2012 Brian Bennett
"
"   Licensed under the Apache License, Version 2.0 (the "License");
"   you may not use this file except in compliance with the License.
"   You may obtain a copy of the License at
"
"       http://www.apache.org/licenses/LICENSE-2.0
"
"   Unless required by applicable law or agreed to in writing, software
"   distributed under the License is distributed on an "AS IS" BASIS,
"   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
"   See the License for the specific language governing permissions and
"   limitations under the License.

" Enable pathogen
execute pathogen#infect()

"""" Pull in local settings before applying global settings
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif

""""
"""" More comfortable working environment
""""

" Tabs suck
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Nicer environment
set ai
set backspace=indent,eol,start
set linebreak
set ruler
set shortmess+=I
set smartcase

" Additional options
set laststatus=2              " always have a status bar
set scrolloff=1               " don't let the curser get too close to the edge

if exists('+colorcolumn')
    set colorcolumn=80
endif

" Do NOT do these things
set nofoldenable
set nohlsearch

" Beautify
syntax enable
set background=dark

" Solarized
"let g:solarized_termcolors=16
let g:solarized_termtrans =1
let g:solarized_degrade   =0
let g:solarized_bold      =0
let g:solarized_underline =0
let g:solarized_italic    =0
let g:solarized_contrast  ="high"
let g:solarized_visibility="high"
colorscheme solarized

" vim-json
" FFS, don't hide things!
let g:vim_json_syntax_conceal = 0

" gist-vim
let g:gist_post_private = 1

" Mark non-ascii characters
match Error /[^ -~\t]/

" Make errors VERY VISIBLE
" Example: “string using non-ascii quotes”
hi Error ctermbg=DarkRed
hi Error ctermfg=Cyan

""""
"""" Remap Commands
""""

" Disable keys that annoy me
nnoremap Q <nop>
nnoremap q <nop>

" Forgot to sudo? Here's the answer.
" This nice little baby I found at http://solidstateraam.com/saving-files-as-root-from-inside-vim/
cmap w!! w !sudo tee % >/dev/null

" Encrypt/Decrypt arbitrary files
" cmap encrypt % !openssl enc -e -aes256 -salt -a
" cmap decrypt % !openssl enc -d -aes256 -salt -a
cmap gpg-e % !gpg2 --encrypt --armor
cmap gpg-d % !gpg2 --decrypt

""""
"""" Automation
""""

" Automatically trim trailing white space on any line.
autocmd BufWritePre * :%s/\s\+$//e

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

" for Bind
au BufRead,BufNewFile db.* set ft=bindzone
au BufRead,BufNewFile zones.conf set ft=named
au BufRead,BufNewFile zones.rfc1918 set ft=named
au BufRead,BufNewFile named.conf.* set ft=named

" Makefile
au FileType make set noexpandtab

" Easy edit and reload
nmap <silent>  ;v  :next $MYVIMRC<CR>
augroup VimReload
    autocmd!
    autocmd BufWritePost  $MYVIMRC  source $MYVIMRC
augroup END

" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Make vim act more like a modern text editor.
set autowrite
set updatecount=10
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undo')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undolevels=5000
    set undofile
endif

" Lightline
set noshowmode
let g:lightline = {
    \ 'colorscheme': 'solarized',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
    \ },
    \ 'component': {
    \   'readonly': '%{&readonly?"🔏":""}',
    \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
    \ },
    \ 'component_visible_condition': {
    \   'readonly': '(&filetype!="help"&& &readonly)',
    \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
    \ },
\ }
