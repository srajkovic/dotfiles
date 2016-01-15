# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/dotfiles/zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/dotfiles/zprezto/init.zsh"
fi

# cd into Development
function cdd {
      cd ~/Development
      if [ -e "$1" ]
      then
              cd $1
      fi

}
compdef '_files -W "$HOME/Development"' cdd

alias be='bundle exec'
alias home="vcsh dotfiles"

source ~/dotfiles/aliases

# run z script
. ~/bin/z.sh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
