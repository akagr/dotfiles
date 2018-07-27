# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify
unsetopt beep nomatch
bindkey -e

bindkey -M viins 'jj' vi-cmd-mode
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/akash/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export EDITOR="vim"
export CLICOLOR=1

source ~/.zprompt

alias servedir="python -m SimpleHTTPServer"
alias gti="git"
alias res="tmux attach -t 0"
jira () {
    git branch -a |\
    grep $1 |\
    head -n 1 |\
    sed 's|* ||' |\
    sed 's|remote/origin/||' |\
    xargs -I {} git checkout {}
}

export NVM_DIR="/Users/akash/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh

###-tns-completion-start-###
if [ -f /Users/akash/.tnsrc ]; then 
    source /Users/akash/.tnsrc 
fi
###-tns-completion-end-###
export PATH="/usr/local/sbin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
