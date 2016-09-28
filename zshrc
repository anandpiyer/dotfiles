#------------------------------------------------------------------------------
# Colors
#------------------------------------------------------------------------------
autoload -U colors && colors
export CLICOLOR="yes"

#------------------------------------------------------------------------------
# Completion
#------------------------------------------------------------------------------
autoload -U compinit && compinit 
zmodload -i zsh/complist
setopt auto_menu
setopt complete_in_word

zstyle ':completion:*' list-colors ''

autoload -U promptinit && promptinit

# http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/
# http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
# https://github.com/myfreeweb/dotfiles/blob/master/zsh/zshrc

#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------
# Use ctrl-z to return to paused Vim instead of 'fg<Enter>'.
# http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
