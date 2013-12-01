#!/bin/zsh
#
# zsh init script execution order:
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

# try to speed up zsh / oh-my-zsh / antigen loading
skip_global_compinit=1


############################################################
# oh-my-zsh
############################################################

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(autojump brew bundler cake compleat cparm dircycle dirpersist extract gem git git-flow github gnu-utils heroku history-substring-search node npm osx rails3 rake ruby sublime terminalapp zsh-syntax-highlighting)


############################################################
# antigen (zsh package manager)
############################################################

source $HOME/.antigen/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen-bundles <<EOBUNDLES

autojump
brew
command-not-found
dircycle
dirpersist
extract
git
gnu-utils
npm
# per-directory-history

# nicoulaj's moar completion files for zsh
zsh-users/zsh-completions src

# ZSH port of Fish shell's history search feature.
zsh-users/zsh-history-substring-search

# Syntax highlighting bundle.
zsh-users/zsh-syntax-highlighting

EOBUNDLES

# Detect Mac OS X
if [[ "$OSTYPE" == "darwin"* ]]; then
  antigen-bundle osx
fi

# Customized Sublime Text plugin (for version 3)
source $HOME/.zsh/plugins/sublime/sublime.plugin.zsh

# Load the theme.
antigen-theme $HOME/.zsh/themes/prose-knosis.zsh-theme

# Tell antigen that you're done.
antigen-apply

# completion
autoload -U compinit
compinit

############################################################
# interactive shell customizations
############################################################


# ZSH ######################################################

# enable zsh emacs mode
bindkey -e



# load our own completion functions
fpath=(~/.zsh/completion $fpath)

# use incremental search
# bindkey "^R" history-incremental-search-backward

# add some readline keys back
# bindkey "^A" beginning-of-line
# bindkey "^E" end-of-line

# handy keybindings
# bindkey "^P" history-search-backward
# bindkey "^Y" accept-and-hold
# bindkey "^N" insert-last-word
bindkey -s "^T" "^[Isudo ^[A" # "t" for "toughguy"

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

# disable auto correct
unsetopt CORRECT CORRECT_ALL

# Enable extended globbing
setopt EXTENDED_GLOB

# Enable zmv command
autoload zmv

# Red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"


# GRC ######################################################
source "`brew --prefix`/etc/grc.bashrc"


# rbenv ####################################################
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# nvm ######################################################
[[ -s /Users/matt/.nvm/nvm.sh ]] && . /Users/matt/.nvm/nvm.sh


# Other Shell Customizations ###############################

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

# use vim as pager / less replacement
export PAGER=vimpager
alias less=$PAGER
# use vim as an editor
export EDITOR=vim


# PATH #####################################################

# include various package management locations in path
export PATH="$HOME/bin:/usr/local/share/npm/bin:/usr/local/lib/node_modules:/usr/local/bin:/usr/local/sbin:$PATH"
