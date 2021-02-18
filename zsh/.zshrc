source $HOME/.env

ZSH_THEME="robbyrussell"

plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

alias zrc="source $ZDOTDIR/.zshrc"

if [ ! $INSIDE_EMACS ]; then
  # vi mode
  bindkey -v
  export KEYTIMEOUT=1

  # Use vim keys in tab complete menu:
  bindkey -M menuselect 'h' vi-backward-char
  bindkey -M menuselect 'k' vi-up-line-or-history
  bindkey -M menuselect 'l' vi-forward-char
  bindkey -M menuselect 'j' vi-down-line-or-history
  bindkey -v '^?' backward-delete-char
  # Change cursor shape for different vi modes.
  function zle-keymap-select {
      if [[ ${KEYMAP} == vicmd ]] ||
           [[ $1 = 'block' ]]; then
        echo -ne '\e[1 q'
      elif [[ ${KEYMAP} == main ]] ||
             [[ ${KEYMAP} == viins ]] ||
             [[ ${KEYMAP} = '' ]] ||
             [[ $1 = 'beam' ]]; then
        echo -ne '\e[5 q'
      fi
  }
  zle -N zle-keymap-select
  zle-line-init() {
      zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
      echo -ne "\e[5 q"
  }
  zle -N zle-line-init
  echo -ne '\e[5 q' # Use beam shape cursor on startup.
  preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.
fi

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
