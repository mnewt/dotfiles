#!/usr/bin/env zsh
# Runs in interactive shells

source "$HOME/bin/shell_utils"
source_if "$HOME/.env"
source_if "$HOME/.aliases"

# If we are in an interactive terminal then use the fancy prompt. Otherwise, use
# a basic one. Scenarios include:
# - xterm like terminals.
# - Emacs shell, which can interpret color codes but doesn't do ncurses. Reports
#   as "dumb".
# - Emacs TRAMP, which processes the prompt using regexp.
if [[ $TERM = *term* ]]; then
	set_bold='%B'
	set_normal=$'\033[0m'

	cmdstatus="%(?..$(set_bg 009)$(set_fg 255) %? )"

	# Only display username and hostname if we are over SSH
	if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
		username="$(set_bg 003)$(set_fg 000) %n "
		hostname="$(set_bg 002)$(set_fg 000) %m "
	else
		username=""
		hostname=""
	fi

	dir="$(set_bg 014)$(set_fg 016) %~ "

	newline=$'\n'
	sigil="${set_normal}${newline}${set_bold}%#${set_normal} %b"

	if [[ $TERM = *vterm* ]]; then
		vterm_part="%{$(vterm_prompt_end)%}"
	else
		vterm_part=""
	fi

	setopt PROMPT_SUBST
	PS1="${cmdstatus}${username}${hostname}${dir}${sigil}${vterm_part}"

else
	PS1="[%n@%m %1~]%# "
fi

# direnv
installed direnv && eval "$(direnv hook bash)"

# rbenv
installed rbenv && eval "$(rbenv init -)"

# nvm
if [ -f "/usr/local/opt/nvm/nvm.sh" ]; then
	mkdir "$HOME/.nvm"
	export NVM_DIR="$HOME/.nvm"
	. "/usr/local/opt/nvm/nvm.sh"
fi
