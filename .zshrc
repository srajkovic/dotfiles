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

_vex() {
    local curcontext="$curcontext" state line
    typeset -A opt_args
    local vpath vbinpath
    local pythons

    pythons=( ${commands[(I)python[0-9].[0-9]|pypy|jython]} )
    _arguments -A "-*" \
	       '(: -)'{-h,--help}'[print help information]' \
	       '(: -)--shell-config[print config for the specified shell]:shell:( bash zsh )' \
	       '--cwd[set working directory for subprocess]:directory:_files -/' \
	       '--config[read config file]:file:_files -g *(.r)' \
	       '(-m --make)'{-m,--make}'[make the named virtualenv before running command]' \
	       '--python[use the named python when making virtualenv]:python:(${pythons})' \
	       '--site-packages[made virtualenv allows access to site packages]' \
	       '--always-copy[copy files instead of making symlinks]' \
	       '(-r --remove)'{-r,--remove}'[remove the named virtualenv after running command]' \
	       '(1)--path[set path to virtualenv]:virtualenv directory:_path_files -/' \
	       '(--path)1:virtualenv:_path_files -/ -W "/Users/stefan/.virtualenvs"' \
	       '2:command:->command_state' \
	       '*::arguments: _normal'

    case $state in
	command_state)
	    vpath="/Users/stefan/.virtualenvs/${line[1]}"
	    vbinpath="$vpath/bin"
	    if [ "$vbinpath" != "/" ] && [ -d "$vbinpath" ]; then
		_alternative \
		    'virtualenvcommand:command in virtualenv:_path_files -W "$vbinpath" -g "*(x-.)"' \
		    '::_command_names -e'
	    fi
	    ;;
	*)
    esac
}
compdef _vex vex

source ~/dotfiles/aliases

. ~/bin/z.sh

if brew command command-not-found-init > /dev/null; then eval "$(brew command-not-found-init)"; fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

fpath=(/usr/local/share/zsh-completions $fpath)

# For NVM
export NVM_DIR="/Users/stefan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# For brew
export PATH="/usr/local/sbin:$PATH"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
