#!/usr/bin/env zsh
# Runs in interactive shells

source "$HOME/bin/shell_interactive_common"

if [[ $INSIDE_EMACS = *vterm* ]]; then
  source_if "$HOME/bin/vterm_utils"
  vterm_part='%{$(vterm_prompt_end)%}'
else
  vterm_part=''
fi

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
  sigil="${set_normal}${newline}${set_bold}%#${set_normal}%b "

  setopt PROMPT_SUBST
  PROMPT="${cmdstatus}${username}${hostname}${dir}${sigil}${vterm_part}"

else
  PROMPT="[%n@%m %1~]%# "
fi
