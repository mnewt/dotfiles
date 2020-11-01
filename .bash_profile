#!/usr/bin/env bash
# Runs in interactive shells

. "$HOME/bin/shell_interactive_common"
. "$HOME/.bashrc"

if [[ $INSIDE_EMACS = *vterm* ]]; then
  . "$HOME/bin/vterm_utils"
  vterm_part='\[$(vterm_prompt_end)\]'
else
  vterm_part=''
fi

# If we are in a smart terminal then use the fancy prompt. Otherwise, use
# a basic one.
#
# TERM can be one of:
# - xterm*
# - eterm*
# - dumb, which might be
#   - Emacs shell, which can interpret color codes.
#   - Emacs TRAMP, which processes the prompt using regexp.
if [[ $TERM = *term* ]]; then
  set_bold='\[\033[1m\]'
  set_normal='\[\033[0m\]'

  cmdstatus='$(s=$? && [ $s != 0 ] && echo "\[$(set_bg 009)$(set_fg 255)\] $s ")'

  # Only display username and hostname if we are over SSH
  if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    username="\[$(set_bg 003)$(set_fg 000)\] \u "
    hostname="\[$(set_bg 002)$(set_fg 000)\] \h "
  else
    username=""
    hostname=""
  fi

  dir="\[$(set_bg 014)$(set_fg 016)\] \w "

  sigil="${set_normal}\n${set_bold}\$${set_normal} "

  PS1="${cmdstatus}${username}${hostname}${dir}${sigil}${vterm_part}"

else
  PS1="[\u@\h \w]$ "
fi

# `bash-completion` and `emacs-bash-completion`
# brew install bash-completion@2
if [[ -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" == "t" ]]; then
  source_if "/usr/local/share/bash-completion/bash_completion"
  source_if "/usr/local/etc/bash_completion"
  source_if "/etc/bash_completion"
fi

# Bash is running inside a terminal emulator.
if [[ -z "$INSIDE_EMACS" ]] || [[ "$INSIDE_EMACS" = *term* ]]; then
  # Bind M-p and M-n to help with Emacs muscle memory.
  bind '"\ep":previous-history'
  bind '"\en":next-history'
fi
