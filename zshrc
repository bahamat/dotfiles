#   Copyright 2017 Brian Bennett
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

# Session support
# See ~/.zshrc_Apple_Terminal
function shell_session_save_user_state() {
    printf 'cd "%s"\n' "${PWD}" >> "$SHELL_SESSION_FILE"
}
[[ -r "$HOME/.zshrc_$TERM_PROGRAM" ]] && source "$HOME/.zshrc_$TERM_PROGRAM"

# Pull in local settings, if any
if [ -f ~/.zshrc.local ]
then
  source ~/.zshrc.local
fi

# Set up additional paths
if [[ "$OSTYPE" == "solaris2.11" ]]
then
  export PATH=/usr/gnu/bin:/usr/bin:/usr/sbin:/sbin
  alias p='pfexec'
else
    alias p='sudo'
fi

MY_PATHS=(
    "$HOME/bin"
    "/opt/pkg/sbin"
    "/opt/pkg/bin"
    "/usr/gnu/bin"
    "/usr/sfw/bin"
    "/usr/X11R6/bin"
    "/sbin"
    "/usr/sbin"
    "/usr/games"
    "/Developer/Tools"
    "/Developer/usr/bin"
    "$HOME/.cabal/bin"
    "$HOME/.npm-global/bin"
    "/usr/local/bin"
    "/usr/local/sbin"
    "/opt/local/bin"
    "/opt/local/sbin"
    "/opt/sfw/bin"
)

for this_path in "${MY_PATHS[@]}"; do
    if [ -d "$this_path" ] && [[ ":$PATH:" != *":$this_path:"* ]]; then
        PATH="$PATH:$this_path"
    fi
done

unset MY_PATHS

# Set user titlebar
case $TERM in
  *term* | xterm-*color | rxvt* | gnome* )
    [ -n "$ZSH_NAME" ] && HOSTNAME=$HOST
    precmd () {
      printf '\e]0;%s\a' "${HOSTNAME//.*/}:${PWD}"
    }
    ;;
  *)
    ;;
esac

# for tmux: export 256color
[ -n "$TMUX" ] && export TERM=screen-256color

# History control
export HISTSIZE=5000
export SAVEHIST=$HISTSIZE
export HISTFILE=~/.history
export HISTCONTROL=ignorespace:erasedups
# See additional ZSH specific options
setopt HIST_BEEP
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE

# Make less more awesome
export PAGER="less --ignore-case --line-numbers --hilite-unread --squeeze-blank-lines --LONG-PROMPT --tabs=3"
alias less="$PAGER"
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_so=$'\E[38;33;246m'   # begin standout-mode - info box
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_zz=$'\E[0m'           # reset

# Other ENV
export EDITOR="vi"
export RSYNC_RSH="ssh"

case $HOSTNAME in
  dbuild*)
    export DEBFULLNAME="Brian Bennett"
    export DEBEMAIL="bahamat@digitalelf.net"
    ;;
  * )
    ;;
esac

alias ll='ls -l'
alias la='ls -a'
alias myip='curl http://ifconfig.me/ip'
alias screen='screen -d -R'
alias shs='python -m SimpleHTTPServer'
alias ssh='ssh -A'
alias vx='VBoxManage'
alias wd='watch --differences=cumulative'
alias wo='find . -user $LOGNAME -perm +0200 -type f | sort'

# Functions

function enc () {
    openssl enc -aes-256-cbc -in "$@"
}

function dec () {
    openssl enc -d -aes-256-cbc -in "$@"
}

function joinstr () {
    # https://stackoverflow.com/a/17841619/818112
    local IFS="$1"; shift
    echo "$*"
}

function murl () {
    echo "${MANTA_URL}${1}"
}

function pkgstat () {
    pkgin -pl '<' ls
}

# Zsh only options
if [ -n "$ZSH_NAME" ]
then

    function setprompt () {
        case $1 in
            default|normal|fancy)
                # set a fancy prompt
                PROMPT="%F{blue}%m:%~]%f "
                RPROMPT="%(?..%F{red}'-> %?%f)"
                ;;
            test|demo)
                PROMPT='%F{blue}>%f '
                RPROMPT=''
                ;;
            emoji)
                PROMPT="%F{blue}💻 %~ 🐚%f "
                RPROMPT="%(?..%F{red}😖 %?%f)"
                ;;
        esac
    }
    if (( ASCIINEMA_REC == 1 )); then
        setprompt demo
    else
        setprompt default
    fi

  setopt HIST_IGNORE_ALL_DUPS # Ignore duplicate entries
  setopt HIST_NO_STORE        # History doesn't save "history"
  setopt HIST_IGNORE_SPACE    # Don't save commands prefixed with space
  setopt HIST_REDUCE_BLANKS   # Squeeze whitespace

  # Use Emacs keybindings.
  bindkey -e

  # Fix ^U
  bindkey ^U backward-kill-line

  # Fix ^W
  autoload -U backward-kill-word-match
  zle -N backward-kill-word backward-kill-word-match
  zstyle ':zle:backward-kill-word' word-style whitespace
  WORDCHARS="${WORDCHARS}|"

  # ZSH completion for ssh
  autoload -U compinit
  compinit
  zstyle ':completion:*' users off
  zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

  if [[ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]
  then
    source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  fi

fi

# If we have go set up, use it.
[[ -f ~/.golang_env ]] && source ~/.golang_env
