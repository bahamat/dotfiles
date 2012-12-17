#   Copyright 2012 Brian Bennett
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

MY_PATHS="
$HOME/bin
/usr/gnu/bin
/usr/sfw/bin
/usr/X11R6/bin
/sbin
/usr/sbin
/usr/games
/Developer/Tools
/Developer/usr/bin
/usr/local/bin
/usr/local/sbin
/opt/local/bin
/opt/local/sbin
/opt/sfw/bin
"

[ -n "$BASH_VERSION" ] && all_paths=${MY_PATHS}
[ -n "$ZSH_NAME" ] && all_paths=(${(f)MY_PATHS})

for this_path in $all_paths; do
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
      echo -ne "\033]0;${USER}@${HOSTNAME//.*/}${PWD}\007"
    }
    PROMPT_COMMAND='precmd'
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
alias myip='lynx -dump http://automation.whatismyip.com/n09230945.asp'
alias ssh='ssh -A'
alias vx='VBoxManage'
alias wo='find . -user $LOGNAME -perm +0200 -type f | sort'

# Bash only options
if [ -n "$BASH_VERSION" ]
then

  # set a fancy prompt
  PS1='\h:\w \u\$ '

  # Make bash check it's window size after a process completes
  shopt -s checkwinsize

  # Turn on bash completion if available
  if [ -f /etc/bash_completion ]; then
     . /etc/bash_completion
  fi

fi

# Zsh only options
if [ -n "$ZSH_NAME" ]
then

  # set a fancy prompt
  PROMPT="%F{blue}%m:%~]%f "
  RPROMPT="%(?..%F{red}%? <-%f)"

  setopt HIST_IGNORE_ALL_DUPS # Ignore duplicate entries
  setopt HIST_NO_STORE        # History doesn't save "history"
  setopt HIST_IGNORE_SPACE    # Don't save commands prefixed with space
  setopt HIST_REDUCE_BLANKS   # Squeeze whitespace

  # Use Emacs keybindings.
  bindkey -e
  bindkey ^U backward-kill-line

  # ZSH completion for ssh
  autoload -U compinit
  compinit
  zstyle ':completion:*' users off
  zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

  if [[ -f ~/repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]
  then
    source ~/repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
  fi

fi
