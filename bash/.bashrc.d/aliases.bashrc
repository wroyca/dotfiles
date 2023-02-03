# .bashrc

alias vi='nvim'
alias vim='nvim'
alias nano='nvim'

set editing-mode vi
set keymap vi
set vi-ins-mode-string "+"
set vi-cmd-mode-string ":"

alias bb="b -vn |& tee compiledb && sed -i 's/c++23/gnu++2b/g' compile_commands.json"
alias bc="b clean"
alias bd="bdep init -C @gcc cc config.cxx=g++"

