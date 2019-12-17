#!/usr/bin/env bash

[ -f "$HOME/.bin/shell_utils" ] && . "$HOME/.bin/shell_utils"
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
[ -f "$HOME/.aliases" ] && . "$HOME/.aliases"

# If we are in an interactive terminal then use the fancy prompt. Otherwise, use
# a basic one. Scenarios include:
# - xterm like terminals.
# - Emacs shell, which can interpret color codes but doesn't do ncurses. Reports
#   as "dumb".
# - Emacs TRAMP, which processes the prompt using regexp.
if test -t 0; then
	set_bold='\033[1m'
	set_normal='\033[0m'

	cmdstatus="\[$(set_bg 001)$(set_fg 255)\]"'$(s=$? && [ $s != 0 ] && echo " $s ")'
	# Only display username and hostname if we are over SSH
	if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
		username="\[$(set_bg 003)$(set_fg 000)\] \u "
		hostname="\[$(set_bg 002)$(set_fg 000)\] \h "
	else
		username=""
		hostname=""
	fi
	dir="\[$(set_bg 014)$(set_fg 016)\] \w "
	sigil="\[${set_normal}\]\n\[${set_bold}\]\$\[${set_normal}\] "

	PS1="${cmdstatus}${username}${hostname}${dir}${sigil}"
else
	PS1="[\u@\h \w]$ "
fi

export PS1

source_if "$HOME/.bin/__prompt"

# `bash-completion` and `emacs-bash-completion`
# brew install bash-completion@2
if [[ -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" == "t" ]]; then
	source_if "/usr/local/share/bash-completion/bash_completion"
	source_if "/usr/local/etc/bash_completion"
	source_if "/etc/bash_completion"

	# Bind M-p and M-n to help with Emacs muscle memory.
	bind '"\ep":previous-history'
	bind '"\en":next-history'
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
