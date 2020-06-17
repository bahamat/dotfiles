#   Copyright 2020 Brian Bennett
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Set up additional paths
if [[ "$OSTYPE" == "solaris2.11" ]]
then
  export PATH=/usr/gnu/bin:/usr/bin:/usr/sbin:/sbin
fi

MY_PATHS=(
    "$HOME/bin"
    "opt/pkg/sbin"
    "/opt/pkg/bin"
    "/usr/gnu/bin"
    "/usr/sfw/bin"
    "/usr/X11R6/bin"
    "/sbin"
    "/usr/sbin"
    "/usr/games"
    "/Developer/Tools"
    "/Developer/usr/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/opt/local/bin"
    "/opt/local/sbin"
    "/opt/sfw/bin"
)

for path in $MY_PATHS; do
  if ! echo $PATH | grep $path >/dev/null ; then
    test -d $path && PATH=$PATH:$path
  fi
done

# Set user titlebar
case $TERM in
  *term* | xterm-*color | rxvt | gnome* )
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME//.*/} : ${PWD}\007"'
    ;;
  *)
    ;;
esac

# Tell the terminal about the working directory at each prompt.
if [ "$TERM_PROGRAM" == "Apple_Terminal" ] && [ -z "$INSIDE_EMACS" ]; then
  update_terminal_cwd() {
    # Identify the directory using a "file:" scheme URL,
    # including the host name to disambiguate local vs.
    # remote connections. Percent-escape spaces.
    local SEARCH=' '
    local REPLACE='%20'
    local PWD_URL="file://$HOSTNAME${PWD//$SEARCH/$REPLACE}"
    printf '\e]7;%s\a' "$PWD_URL"
  }
  PROMPT_COMMAND="update_terminal_cwd${PROMPT_COMMAND:+; $PROMPT_COMMAND}"
fi

# If this is bash, emulate zsh's missing newline marker
if [[ -n BASH_VERSION ]]; then
    PROMPT_COMMAND='printf "%%%$((COLUMNS-1))s\\r";'"$PROMPT_COMMAND"
fi

# set a fancy prompt
PS1='\h:\w \u\$ '

# for tmux: export 256color
[ -n "$TMUX" ] && export TERM=screen-256color

# Turn on bash completion if available
if [ $BASH ] && [ -f /etc/bash_completion ]; then
   . /etc/bash_completion
fi

# History control
# if ! [ -d ~/.histories/ ]; then
#    mkdir ~/.histories/
# fi
export HISTCONTROL=ignorespace:erasedups
# export HISTFILE=$HOME/.histories/$(echo ${HOSTNAME//.*/} | tr "[:upper:]" "[:lower:]")

# Set env
# export CLICOLOR=1
case ${OSTYPE} in
  darwin*)
    unset MANPATH
    export MANPATH
    ;;
  solaris*)
    export MANPATH=$(echo $PATH | sed 's/bin/man/g')
    ;;
esac

#export PAGER="less -ins"
export PAGER="less --ignore-case --line-numbers --hilite-unread --squeeze-blank-lines --LONG-PROMPT --tabs=3"
export EDITOR=vi
export RSYNC_RSH=ssh

case $TERM_PROGRAM in
  Apple_Terminal)
    export GIT_EDITOR='see -j "Git Commit Message" -w'
    export SVN_EDITOR='see -j "SVN Commit Message" -w'
    ;;
  *)
    export GIT_EDITOR=vi
    export SVN_EDITOR=vi
    ;;
esac

case $HOSTNAME in
  skybyte*)
    export DEBFULLNAME="Brian Bennett"
    export DEBEMAIL=brian.bennett@ntrepidcorp.com
    ;;
  dbuild*)
    export DEBFULLNAME="Brian Bennett"
    export DEBEMAIL=bahamat@digitalelf.net
    ;;
  *)
    ;;
esac

# Aliases
alias ipsort='sort -n -t . -k 1,1 -k 2,2 -k 3,3 -k 4,4'
alias ll='ls -l'
alias la='ls -a'
alias less="$PAGER"
alias myip='lynx -dump http://automation.whatismyip.com/n09230945.asp'
alias osiris='osiris -f /etc/ssl/certs/ca-anon.cert'
alias ssh='ssh -A'
alias wo='find . -user $LOGNAME -perm +0200 -type f | sort'
if [[ ${OSTYPE} =~ "darwin" ]]
then
   alias netstat='netstat -f inet'
fi

# Shell Functions
randword () {
  n=$(cat /usr/share/dict/words | wc -l)
  cat -n /usr/share/dict/words | grep -w $(jot -r 1 1 $n) | cut -f2
}
sniff6 () {
  sudo tcpdump -p $* ip6 and net not fe80::/64
}

# Unlock my ssh keys
if [[ $OSTYPE =~ darwin ]]; then
    ssh-add -l >/dev/null || ssh -add -A 2>/dev/null
fi

# Make bash check it's window size after a process completes
shopt -s checkwinsize

# Less Colors for Man Pages
# http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;33;246m'   # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# If we have go set up, use it.
[[ -f ~/.golang_env ]] && source ~/.golang_env
