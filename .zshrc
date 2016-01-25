# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -e
bindkey '\e.' insert-last-word
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/akash/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export EDITOR="vim"
export CLICOLOR=1

source ~/.zprompt

export NVM_DIR="/Users/akash/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
