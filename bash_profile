#!/bin/bash

# simple prompt
# export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\] at \[\e[0m\]\[\e[00;32m\]\h\[\e[0m\]\[\e[00;37m\] in \[\e[0m\]\[\e[00;34m\]\w\[\e[0m\]\[\e[00;37m\] \\$\[\e[0m\] "

source_if () {
  [ -e "$1" ] && . "$1"
}

source_if "$HOME/.profile"
source_if "$HOME/.bashrc"
source_if "$HOME/.aliases"
source_if "$HOME/.private.sh"
source_if "$HOME/.bin/bash_prompt"
