# Created by newuser for 5.8
source ~/.dotfiles/aliases.sh
source ~/.dotfiles/functions.sh
source ~/.dotfiles/exports.sh
[ -x $(command -v direnv) ] && eval "$(direnv hook zsh)"
#eval `ssh-agent` # Autostart ssh-agent
#source <(summon --bash-completion-script `which summon`)
bindkey '^[[Z' autosuggest-accept  # shift + tab  | autosuggest
