#!/bin/bash
# Runs in interactive, non-login shells

[ -f "$HOME/.env" ] && . "$HOME/.env"

source "$HOME/bin/shell_interactive_common"

if [[ $INSIDE_EMACS == *vterm* ]]; then
  source_if "$HOME/bin/vterm_utils"
  vterm_part='$(vterm_prompt_end)'
else
  vterm_part=''
fi

# If we are in an interactive terminal then use the fancy prompt
if [[ $TERM == *term* ]]; then
  set_bold='\[\e[1m\]'
  set_normal='\[\e[0m\]'

  cmdstatus='$(if [[ $? -ne 0 ]]; then echo -n "$(set_bg 009)$(set_fg 255) $? "; fi)'

  # Only display username and hostname if over SSH
  if [[ -n "$SSH_CLIENT" || -n "$SSH_TTY" ]]; then
    username="$(set_bg 003)$(set_fg 000) \u "
    hostname="$(set_bg 002)$(set_fg 000) \h "
  else
    username=""
    hostname=""
  fi

  # Display Python venv
  if [[ -n "$VIRTUAL_ENV_PROMPT" ]]; then
    virtualenv="$(set_bg 004)$(set_fg 015)[${VIRTUAL_ENV_PROMPT}]"
  else
    virtualenv=""
  fi

  dir="$(set_bg 189)$(set_fg 016) \w "

  newline='\n'
  sigil="\[${set_normal}\]${newline}\[${set_bold}\]\\$\[${set_normal}\] "

  # In Bash, PS1 can evaluate variables and commands with $(...)
  PS1="${cmdstatus}${virtualenv}${username}${hostname}${dir}${sigil}${vterm_part}"

else
  PS1="[\u@\h \W]\\$ "
fi
