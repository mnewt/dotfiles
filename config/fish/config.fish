#!/usr/local/bin/fish

# set -x fish_greeting ''



# Setup terminal, and turn on colors
set -x TERM xterm-256color
set -x LANG en_US.UTF-8
set -x LC_CTYPE "en_US.UTF-8"
set -x LC_MESSAGES "en_US.UTF-8"
set -x LC_COLLATE C

# aliases
if test -e "$HOME/.config/fish/aliases.fish"
  source "$HOME/.config/fish/aliases.fish"
end

# private
if test -e "$HOME/.private.fish"
  source "$HOME/.private.fish"
end

# path
# Homebrew installs to /usr/local/sbin and /usr/local/opt/coreutils/libexec/gnubin
set -e fish_user_paths
for path in "$HOME/.bin" "/usr/local/sbin" "/usr/local/opt/coreutils/libexec/gnubin"
  if test -d "$path"
    set -U fish_user_paths $fish_user_paths "$path"
  end
end

# minicom
set -x MINICOM '-c on'

# python venv
set -x VIRTUAL_ENV_DISABLE_PROMPT true

# virtualenv / virtualfish
# requires virtualfish. Install with:
# pip install virtualfish
# if which virtualenv 2>&1 >/dev/null
#   eval (python -m virtualfish auto_activation)
# end

# Print pretty fish logo only if starting on a local machine
if test -z "$SSH_CLIENT"
	logo
end