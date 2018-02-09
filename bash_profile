#!/bin/bash
# Gets sourced on a login shell

source_if () {
  [ -e "$1" ] && . "$1"
}

# macOS only sources ~/.bash_profile, not ~/.bashrc
source_if "$HOME/.bashrc"

# Use ~/.profile for customizations that should not make it back to repo
source_if "$HOME/.profile"
source_if "$HOME/.bin/start-ssh-agent"
