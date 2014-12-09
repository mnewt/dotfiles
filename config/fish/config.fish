#!/usr/local/bin/fish

set -g -x fish_greeting ''

set -x PATH /usr/local/opt/coreutils/libexec/gnubin $HOME/.bin /usr/local/bin /usr/bin /bin /usr/sbin /sbin

set -gx MANPATH :/usr/local/opt/coreutils/libexec/gnuman

# Set where to install casks
set -x HOMEBREW_CASK_OPTS "--appdir=/Applications"

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

# use vim as an editor
set -xU EDITOR=vim

# use vim as pager / less replacement
if hash vimpager >/dev/null 2>&1
  set -xU PAGER (which vimpager)
  alias less $PAGER
end

# additional functions
source functions/z.fish


# vagrant ##################################################
set -x VAGRANT_DEFAULT_PROVIDER=parallels