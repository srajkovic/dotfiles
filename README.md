# Stefan Rajkovic's dotfiles
## Super Rough Ubuntu Version. These are like, chopped apart from the OSX version on master. I'd recommend against them, but I needed some quick fixes

These are my dotfiles. There are many like them, but these are mine.

Feel free to reach out with any comments or questions!

Setup code
```
# Setup Brew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Clone me :)
git clone --recursive https://github.com/srajkovic/dotfiles.git ~/dotfiles

source ~/dotfiles/setup
```

That should get most everything set up.

## TODO
Make it independent of dotfiles location?
Should just be removing hard codes in prezto locations from init.zsh and .zshrc
And finding a way to persist the location.
