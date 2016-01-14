# Stefan Rajkovic's dotfiles

These are my dotfiles. There are many like them, but these are mine.

Feel free to reach out with any comments or questions!

Setup code (assuming brew is installed)
```
   brew install vsch
   vsch clone https://github.com/srajkovic/dotfiles.git
   brew bundle
   zsh
   git clone --recursive https://github.com/srajkovic/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
   chsh -s /bin/zsh
   setopt EXTENDED_GLOB
   for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
     ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
   done
   cd ~/.emacs.d
   cask
```

That should get most everything set up. I hope.