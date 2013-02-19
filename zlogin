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
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="prose-knosis"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(autojump brew bundler cake compleat cparm dircycle dirpersist extract gem git git-flow github gnu-utils heroku history-substring-search node npm osx rails3 rake ruby rvm sublime terminalapp zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

############################################################
# interactive shell customizations
############################################################


# ZSH
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

# GRC
source "`brew --prefix`/etc/grc.bashrc"


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

# include various package management locations in path
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="/usr/local/share/npm/bin:$PATH"
export PATH="/usr/local/share/python:$PATH"
export PATH="$HOME/bin:$PATH"

# npm packages
NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"

# rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
