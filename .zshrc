#!/usr/bin/env zsh
# Runs in interactive shells

[ -f "$HOME/.bin/shell_utils" ] && source "$HOME/.bin/shell_utils"
[ -f "$HOME/.env" ] && source "$HOME/.env"
[ -f "$HOME/.aliases" ] && source "$HOME/.aliases"

# If we are in an interactive terminal then use the fancy prompt. Otherwise, use
# a basic one. Scenarios include:
# - xterm like terminals.
# - Emacs shell, which can interpret color codes but doesn't do ncurses. Reports
#   as "dumb".
# - Emacs TRAMP, which processes the prompt using regexp.
if test -t 0; then
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
	sigil="${set_normal}\\n${set_bold}%#%{$reset_color%} "
	newline=$'\n'
	sigil="${set_normal}${newline}${set_bold}%# %b"

	setopt PROMPT_SUBST
	PS1="${cmdstatus}${username}${hostname}${dir}${sigil}%{$(vterm_prompt_end)%}"

else
	PS1="[%n@%m %1~]%# "
fi
