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
# thoughtbot
############################################################


# adds the current branch name in green
# git_prompt_info() {
#   ref=$(git symbolic-ref HEAD 2> /dev/null)
#   if [[ -n $ref ]]; then
#     echo "[%{$fg_bold[magenta]%}${ref#refs/heads/}%{$reset_color%}]"
#   fi
# }

# makes color constants available
autoload -U colors
colors

# expand functions in the prompt
# setopt prompt_subst

# prompt
# export PS1='$(git_prompt_info)[${SSH_CONNECTION+"%{$fg_bold[green]%}%n@%m:"}%{$fg_bold[blue]%}%1~%{$reset_color%}] '


############################################################
# oh-my-zsh
############################################################
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="prose-vcprompt"

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
plugins=(autojump brew bundler cake compleat cparm dircycle dirpersist extract gem git git-flow github gnu-utils heroku history-substring-search node npm osx rails3 rake ruby rvm screen sublime terminalapp)

source $ZSH/oh-my-zsh.sh

############################################################
# interactive shell customizations
############################################################

# ZSH
# disable auto correct
unsetopt CORRECT CORRECT_ALL

# enable zsh emacs mode
bindkey -e

# load private environment variables
if [ -e "$HOME/.private" ]; then
  source "$HOME/.private"
fi

alias dom='nocorrect dom '

# add formatting and color to `ls`
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad
alias ls='ls -Fh'
alias ll='ls -Fhl'

# use vim as pager / less replacement
export PAGER=vimpager
alias less=$PAGER

# include various package management locations in path
export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
export PATH="/usr/local/share/npm/bin:$PATH"
export PATH="/usr/local/share/python:$PATH"
export PATH="$HOME/bin:$HOME/.bin:$PATH"

# npm packages
NODE_PATH="$NODE_PATH:/usr/local/lib/node_modules"

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

