export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="custom-ys"
DEFAULT_USER="stefanrajkovic"

export UPDATE_ZSH_DAYS=7
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(brew bundle colored-man compleat gem git gitfast git-extras iterm lol osx pip pod rails ruby)
source $ZSH/oh-my-zsh.sh

# User configuration
export PATH="\
$HOME/anaconda/bin:\
/usr/local/bin:\
$HOME/bin:\
$HOME/.rvm/bin\
/usr/local/mysql/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
/usr/local/bin:\
/opt/X11/bin:\
/usr/local/git/bin:\
/usr/texbin:\
"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export EDITOR='emacs'
export SSH_KEY_PATH="~/.ssh/id_rsa"

# OPAM configuration
. /Users/stefan/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# link my theme in, since .oh-my-zsh is already under VC and I don't want to mess with that
if [[ ! -f ~/.setup-previously ]]; then
	first-time
	touch ~/.setup-previously
fi

# cd into Development
function cdd() {
	cd ~/Development
	if [ -e "$1" ]
	then
		cd $1
	fi	
}

# aliases
source ~/.aliases

# run z script
. ~/bin/z.sh

# chruby setup
source /usr/local/share/chruby/chruby.sh
source /usr/local/opt/chruby/share/chruby/auto.sh

# thefuck setup
eval "$(thefuck --alias)"
