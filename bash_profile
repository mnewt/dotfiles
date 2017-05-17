#!/bin/bash

# simple prompt (fallback)
export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\] at \[\e[0m\]\[\e[00;32m\]\h\[\e[0m\]\[\e[00;37m\] in \[\e[0m\]\[\e[00;34m\]\w\[\e[0m\]\[\e[00;37m\] \\$\[\e[0m\] "

source_if () {
  [ -e "$1" ] && . "$1"
}

source_if "$HOME/.profile"
source_if "$HOME/.bashrc"
source_if "$HOME/.aliases"
source_if "$HOME/.private.sh"
source_if "$HOME/.bin/bash_prompt"

source_if "$HOME/.bin/start-ssh-agent"

[ "$TERM_PROGRAM" = "iTerm.app" ] && \
  source_if "$HOME/.bin/iterm2_shell_integration.bash"

if installed direnv; then
  eval "$(direnv hook bash)"
fi

if installed rbenv; then
  eval "$(rbenv init -)"
fi

#fzf
source_if "$HOME/.fzf/shell/completion.bash"
# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir" || return
}
