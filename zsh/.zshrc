HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

zstyle :compinstall filename '/home/wroy/.zshrc'
autoload -Uz compinit
compinit

bindkey -v # vi key bindings

eval "$(starship init zsh)"
