" openssl.vim version 3.3 2008 Noah Spurrier <noah@noah.org>
"
" == Edit OpenSSL encrypted files and turn Vim into a Password Safe! ==
"
" This plugin enables reading and writing of files encrypted using OpenSSL.
" The file must have the extension of one of the ciphers used by OpenSSL.
" For example:
"
"    .des3 .aes .bf .bfa .idea .cast .rc2 .rc4 .rc5
"
" This will turn off the swap file and the .viminfo log. The `openssl` command
" line tool must be in the path.
"
" == Install ==
"
" Put this in your plugin directory and Vim will automatically load it:
"
"    ~/.vim/plugin/openssl.vim
"
" You can start by editing an empty unencrypted file. Give it one of the
" extensions above. When you write the file you will be asked to give it a new
" password.
"
" == Simple Vim Password Safe ==
"
" If you edit any file named '.auth.aes' (that's the full name, not just the
" extension) then this plugin will add folding features and an automatic quit
" timeout.
"
" Vim will quit automatically after 5 minutes of no typing activity (unless
" the file has been changed).
"
" This plugin will fold on wiki-style headlines in the following format:
"
"     == This is a headline ==
"
" Any notes under the headline will be inside the fold until the next headline
" is reached. The SPACE key will toggle a fold open and closed. The q key will
" quit Vim. Create the following example file named ~/.auth.aes:
"
"     == Colo server ==
"
"     username: maryjane password: esydpm
"
"     == Office server ==
"
"     username: peter password: 4m4z1ng
"
" Then create this bash alias:
"
"     alias auth='view ~/.auth.aes'
"
" Now you can view your password safe by typing 'auth'. When Vim starts all
" the password information will be hidden under the headlines. To view the
" password information put the cursor on the headline and press SPACE. When
" you write an encrypted file a backup will automatically be made.
"
" FIXME This plugin can also make a backup of an encrypted file before writing
" FIXME changes. This helps guard against the situation where you may edit a file
" FIXME and write changes with the wrong password. You can still go back to the
" FIXME previous backup version. The backup file will have the same name as the
" FIXME original file with .bak appended. For example:
"
" FIXME     .auth.aes  -->  .auth.aes.bak
" FIXME
" FIXME Backups are NOT made by default. To turn on backups put the following global
" FIXME definition in your .vimrc file:
" FIXME
" FIXME     let g:openssl_backup = 1
"
" Thanks to Tom Purl for the original des3 tip.
"
" I release all copyright claims. This code is in the public domain.
" Permission is granted to use, copy modify, distribute, and sell this
" software for any purpose. I make no guarantee about the suitability of this
" software for any purpose and I am not liable for any damages resulting from
" its use. Further, I am under no obligation to maintain or extend this
" software. It is provided on an 'as is' basis without any expressed or
" implied warranty.
"

augroup openssl_encrypted
if exists("openssl_encrypted_loaded")
    finish
endif
let openssl_encrypted_loaded = 1
autocmd!

function! s:OpenSSLReadPre()
    if has("filterpipe") != 1
        echo "Your systems sucks."
        exit 1
    endif
    set secure
    set cmdheight=3
    set viminfo=
    set clipboard=
    set noswapfile
    set noshelltemp
    set shell=/bin/sh
    set bin
endfunction

function! s:OpenSSLReadPost()
    " Most file extensions can be used as the cipher name, but
    " a few  need a little cosmetic cleanup.
    let l:cipher = expand("%:e")
    if l:cipher == "aes"
        let l:cipher = "aes-256-cbc -a"
    endif
    if l:cipher == "bfa"
        let l:cipher = "bf -a"
    endif
    let l:expr = "0,$!openssl " . l:cipher . " -d -salt -pass stdin -in " . expand("%")

    set undolevels=-1
    let l:a = inputsecret("Password: ")
    " Replace encrypted text with the password to be used for decryption.
    execute "0,$d"
    execute "normal i". l:a
    " Replace the password with the decrypted file.
    silent! execute l:expr
    " Cleanup.
    let l:a="These are not the droids you're looking for."
    set undolevels&
    redraw!
    if v:shell_error
        silent! 0,$y
        silent! undo
        redraw!
        echo "ERROR -- COULD NOT DECRYPT"
        echo "You may have entered the wrong password or"
        echo "your version of openssl may not have the given"
        echo "cipher engine built-in. This may be true even if"
        echo "the cipher is documented in the openssl man pages."
        echo "DECRYPT EXPRESSION: " . l:expr
        echo "Press any key to continue..."
        let char = getchar()
        return
    endif
    set nobin
    set cmdheight&
    set shell&
    execute ":doautocmd BufReadPost ".expand("%:r")
    redraw!
endfunction

function! s:OpenSSLWritePre()
    set cmdheight=3
    set shell=/bin/sh
    set bin

    " FIXME: I think there is a bug in Vim... dont' use this.
"    if !exists("g:openssl_backup")
"        let g:openssl_backup=0
"    endif
"    if (g:openssl_backup)
"        silent! execute '!cp % %.bak'
"    endif

    " Most file extensions can be used as the cipher name, but
    " a few  need a little cosmetic cleanup. AES could be any flavor,
    " but I assume aes-256-cbc format with base64 ASCII encoding.
    let l:cipher = expand("<afile>:e")
    if l:cipher == "aes"
        let l:cipher = "aes-256-cbc -a"
    endif
    if l:cipher == "bfa"
        let l:cipher = "bf -a"
    endif
    let l:expr = "0,$!openssl " . l:cipher . " -e -salt -pass stdin"

    let l:a  = inputsecret("       New password: ")
    let l:ac = inputsecret("Retype new password: ")
    if l:a != l:ac
        " This gives OpenSSLWritePost something to UNDO..
    silent! execute "0goto"
    silent! execute "normal iThis file has not been saved.\n"
        let l:a ="These are not the droids you're looking for."
        let l:ac="These are not the droids you're looking for."
        echo "ERROR -- COULD NOT ENCRYPT"
        echo "The new password and the confirmation password did not match."
        echo "ERROR -- COULD NOT ENCRYPT"
        echo "Press any key to continue..."
        redraw!
        let char = getchar()
        return 1
    endif
    silent! execute "0goto"
    silent! execute "normal i". l:a . "\n"
    silent! execute l:expr
    " Cleanup.
    let l:a ="These are not the droids you're looking for."
    let l:ac="These are not the droids you're looking for."
    redraw!
    if v:shell_error
        silent! 0,$y
        " Undo the encryption.
        silent! undo
        redraw!
        echo "ERROR -- COULD NOT ENCRYPT"
        echo "Your version of openssl may not have the given"
        echo "cipher engine built-in. This may be true even if"
        echo "the cipher is documented in the openssl man pages."
        echo "ENCRYPT EXPRESSION: " . expr
        echo "ERROR FROM OPENSSL:"
        echo @"
        echo "ERROR -- COULD NOT ENCRYPT"
        echo "Press any key to continue..."
        let char = getchar()
        return 1
    endif
endfunction

function! s:OpenSSLWritePost()
    " Undo the encryption.
    silent! undo
    set nobin
    set shell&
    set cmdheight&
    redraw!
endfunction

autocmd BufReadPre,FileReadPre     *.des3,*.des,*.bf,*.bfa,*.aes,*.idea,*.cast,*.rc2,*.rc4,*.rc5,*.desx call s:OpenSSLReadPre()
autocmd BufReadPost,FileReadPost   *.des3,*.des,*.bf,*.bfa,*.aes,*.idea,*.cast,*.rc2,*.rc4,*.rc5,*.desx call s:OpenSSLReadPost()
autocmd BufWritePre,FileWritePre   *.des3,*.des,*.bf,*.bfa,*.aes,*.idea,*.cast,*.rc2,*.rc4,*.rc5,*.desx call s:OpenSSLWritePre()
autocmd BufWritePost,FileWritePost *.des3,*.des,*.bf,*.bfa,*.aes,*.idea,*.cast,*.rc2,*.rc4,*.rc5,*.desx call s:OpenSSLWritePost()

"
" The following implements a simple password safe for any file named
" '.auth.aes'. The file is encrypted with AES and base64 ASCII encoded.
" Folding is supported for == headlines == style lines.
"

function! HeadlineDelimiterExpression(lnum)
    if a:lnum == 1
        return ">1"
    endif
    return (getline(a:lnum)=~"^\\s*==.*==\\s*$") ? ">1" : "="
endfunction
autocmd BufReadPost,FileReadPost   .auth.aes set foldexpr=HeadlineDelimiterExpression(v:lnum)
autocmd BufReadPost,FileReadPost   .auth.aes set foldlevel=0
autocmd BufReadPost,FileReadPost   .auth.aes set foldcolumn=0
autocmd BufReadPost,FileReadPost   .auth.aes set foldmethod=expr
autocmd BufReadPost,FileReadPost   .auth.aes set foldtext=getline(v:foldstart)
autocmd BufReadPost,FileReadPost   .auth.aes nnoremap <silent><space> :exe 'silent! normal! za'.(foldlevel('.')?'':'l')<CR>
autocmd BufReadPost,FileReadPost   .auth.aes nnoremap <silent>q :q<CR>
autocmd BufReadPost,FileReadPost   .auth.aes highlight Folded ctermbg=red ctermfg=black
autocmd BufReadPost,FileReadPost   .auth.aes set updatetime=300000
autocmd CursorHold                 .auth.aes quit

" End of openssl_encrypted
augroup END
