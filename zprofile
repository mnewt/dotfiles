#!/bin/zsh

# enable zsh online help
unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

# enable zsh emacs mode
bindkey -e

# use incremental search
bindkey "^R" history-incremental-search-backward

# add some readline keys back
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# handy keybindings
bindkey "^P" history-search-backward
bindkey "^Y" accept-and-hold
bindkey "^N" insert-last-word
bindkey -s "^T" "^[Isudo ^[A" # "t" for "toughguy"

# ignore duplicate history entries
setopt histignoredups

# keep TONS of history
export HISTSIZE=40960

# automatically pushd
setopt auto_pushd
export dirstacksize=5

# awesome cd movements from zshkit
setopt AUTOCD
setopt AUTOPUSHD PUSHDMINUS PUSHDSILENT PUSHDTOHOME
setopt cdablevars

# disable auto correct
unsetopt CORRECT CORRECT_ALL

# Enable extended globbing
setopt EXTENDED_GLOB

# Enable zmv command
autoload zmv

# Red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"


# prompt ###################################################

source_if () {
  [ -e "$1" ] && . "$1"
}

source_if "$HOME/.profile"
source_if "$HOME/.bashrc"
source_if "$HOME/.aliases"
source_if "$HOME/.private.sh"
source_if "$HOME/.bin/bash_prompt"
precmd() { eval "$PROMPT_COMMAND" }
PROMPT="$(with_color 000 255 "%% ")"

# iTerm2
if [ "$TERM_PROGRAM" = "iTerm.app" ]; then
  source_if "$HOME/.iterm2/iterm2_shell_integration.sh"
  alias imgcat="$HOME/.iterm2/imgcat"
  alias it2dl="$HOME/.iterm2/it2dl"
fi

if installed direnv; then
  eval "$(direnv hook zsh)"
fi

if [ "$TERM_PROGRAM" = "iTerm.app" ]; then
  . "$HOME/.bin/iterm2_shell_integration.zsh"
fi

source_if "$HOME/.bin/start-ssh-agent"

[ -e "$HOME/.fzf/shell/completion.zsh" ] && . "$HOME/.fzf/shell/completion.zsh"
