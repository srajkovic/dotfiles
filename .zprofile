# LESS options
export LESS='-g -i -M -R -S -w -X -z-4'

export CAML_LD_LIBRARY_PATH="~/.opam/system/lib/stublibs"

# Setting PATH for Python 3.5
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.5/bin:${PATH}"
PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH

eval `opam config env`
