#!/usr/local/bin/fish

set -gx fish_greeting ''

# Setup terminal, and turn on colors
set -x TERM xterm-256color
set -x LANG en_US.UTF-8
set -x LC_CTYPE "en_US.UTF-8"
set -x LC_MESSAGES "en_US.UTF-8"
set -x LC_COLLATE C

# load private environment variables
if test -e $HOME"/.private.fish"
  source $HOME"/.private.fish"
end

# aliases
if test -e $HOME"/.config/fish/aliases.fish"
  source $HOME"/.config/fish/aliases.fish"
end

# path
if test -d $HOME"/.bin"
  set -x PATH ./bin $PATH
end