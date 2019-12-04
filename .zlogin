# Set ENV variables here, for tools like Vex. I don't want every shell that ever spawns to have this stuff, just the one I interact with, aka a login shell.

PATH="/usr/local/bin:$HOME/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/git/bin:/Library/TeX/texbin:$PATH"
EDITOR='emacs'
SSH_KEY_PATH="~/.ssh/id_rsa"
HOMEBREW_CASK_OPTS="--appdir=/Applications"

# iTerm setup
test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
