# Created by newuser for 5.8
source ~/.dotfiles/aliases.sh
source ~/.dotfiles/functions.sh
source ~/.dotfiles/exports.sh
eval "$(direnv hook zsh)"
source <(summon --bash-completion-script `which summon`)
