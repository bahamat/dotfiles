if [[ $OSTYPE =~ darwin ]]; then
    ssh-add -l >/dev/null || ssh-add -A 2>/dev/null
fi
export GPG_TTY=$(tty)
