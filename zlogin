#!/bin/zsh
# +----------------+-----------+-----------+------+
# |                |Interactive|Interactive|Script|
# |                |login      |non-login  |      |
# +----------------+-----------+-----------+------+
# |/etc/zshenv     |    A      |    A      |  A   |
# +----------------+-----------+-----------+------+
# |~/.zshenv       |    B      |    B      |  B   |
# +----------------+-----------+-----------+------+
# |/etc/zprofile   |    C      |           |      |
# +----------------+-----------+-----------+------+
# |~/.zprofile     |    D      |           |      |
# +----------------+-----------+-----------+------+
# |/etc/zshrc      |    E      |    C      |      |
# +----------------+-----------+-----------+------+
# |~/.zshrc        |    F      |    D      |      |
# +----------------+-----------+-----------+------+
# |/etc/zlogin     |    G      |           |      |
# +----------------+-----------+-----------+------+
# |~/.zlogin       |    H      |           |      |
# +----------------+-----------+-----------+------+
# |                |           |           |      |
# +----------------+-----------+-----------+------+
# |                |           |           |      |
# +----------------+-----------+-----------+------+
# |~/.zlogout      |    I      |           |      |
# +----------------+-----------+-----------+------+
# |/etc/zlogout    |    J      |           |      |
# +----------------+-----------+-----------+------+



############################################################
# oh-my-zsh
############################################################

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(autojump brew bundler cake compleat cparm dircycle dirpersist extract gem git git-flow github gnu-utils heroku history-substring-search node npm osx rails3 rake ruby rvm sublime terminalapp zsh-syntax-highlighting)


############################################################
# antigen (zsh package manager)
############################################################
source $HOME/code/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen-lib

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen-bundles <<EOBUNDLES

autojump
brew
# bundler
command-not-found
compleat
dircycle
dirpersist
extract
# gem
git
github
gnu-utils
# heroku
history-substring-search
history
# jake-node
last-working-dir
# mercurial
node
npm
osx
per-directory-history
# pip
# python
# rails
# rails3
# rbenv
rsync
# ruby
themes
urltools

EOBUNDLES

# nicoulaj's moar completion files for zsh
antigen-bundle zsh-users/zsh-completions src

# ZSH port of Fish shell's history search feature.
antigen-bundle zsh-users/zsh-history-substring-search

# Syntax highlighting bundle.
antigen-bundle zsh-users/zsh-syntax-highlighting

# Load my custom plugins
antigen-bundle $HOME/.zsh/plugins

# Load the theme.
antigen-theme $HOME/.zsh/themes/prose-knosis.zsh-theme

# Tell antigen that you're done.
antigen-apply



############################################################
# interactive shell customizations
############################################################


# ZSH ######################################################
# disable auto correct
unsetopt CORRECT CORRECT_ALL

# enable zsh emacs mode
bindkey -e

# makes color constants available
autoload -U colors
colors

# completion
autoload -U compinit
compinit

# load our own completion functions
fpath=(~/.zsh/completion $fpath)

# automatically enter directories without cd
setopt auto_cd

# use incremental search
bindkey "^R" history-incremental-search-backward

# add some readline keys back
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# handy keybindings
bindkey "^P" history-search-backward
bindkey "^Y" accept-and-hold
bindkey "^N" insert-last-word
bindkey -s "^T" "^[Isudo ^[A" # "t" for "toughguy"

# expand functions in the prompt
# setopt prompt_subst

# ignore duplicate history entries
setopt histignoredups

# keep TONS of history
export HISTSIZE=40960

# automatically pushd
setopt auto_pushd
export dirstacksize=5

# awesome cd movements from zshkit
setopt AUTOCD
setopt AUTOPUSHD PUSHDMINUS PUSHDSILENT PUSHDTOHOME
setopt cdablevars

# Try to correct command line spelling
#setopt CORRECT CORRECT_ALL

# Enable extended globbing
setopt EXTENDED_GLOB

# Enable zmv command
autoload zmv

# Red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"


# GRC ######################################################
source "`brew --prefix`/etc/grc.bashrc"


# rbenv #####################################################
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi


# Other Shell Customizations ################################

# load private environment variables
if [ -e "$HOME/.private" ]; then
  source "$HOME/.private"
fi

# aliases
if [ -e "$HOME/.aliases" ]; then
  source "$HOME/.aliases"
fi

# add formatting and color to `ls`
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
alias ls='ls -Fh'
alias ll='ls -Fhl'

# use vim as pager / less replacement
export PAGER=vimpager
alias less=$PAGER
# use vim as an editor
export EDITOR=vim


# PATH #####################################################

# include various package management locations in path
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="/usr/local/share/npm/bin:$PATH"
export PATH="/usr/local/share/python:$PATH"
export PATH="$HOME/bin:$PATH"

# npm packages
NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"


