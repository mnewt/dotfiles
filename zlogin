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


############################################################
# oh-my-zsh
############################################################

# try to speed up zsh / oh-my-zsh / antigen loading
#skip_global_compinit=1

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# plugins=(autojump brew bundler cake compleat cparm dircycle dirpersist extract gem git git-flow github gnu-utils heroku history-substring-search node npm osx rails3 rake ruby sublime terminalapp zsh-syntax-highlighting)


############################################################
# antigen (zsh package manager)
############################################################

# source $HOME/.antigen/antigen/antigen.zsh

# # Load the oh-my-zsh's library.
# antigen use oh-my-zsh

# # Bundles from the default repo (robbyrussell's oh-my-zsh).
# antigen-bundles <<EOBUNDLES

# autojump
# brew
# command-not-found
# dircycle
# dirpersist
# extract
# git
# gnu-utils
# npm
# # per-directory-history

# # nicoulaj's moar completion files for zsh
# zsh-users/zsh-completions src

# # ZSH port of Fish shell's history search feature.
# zsh-users/zsh-history-substring-search

# # Syntax highlighting bundle.
# zsh-users/zsh-syntax-highlighting

# EOBUNDLES

# # Detect Mac OS X
# if [[ "$OSTYPE" == "darwin"* ]]; then
#   antigen-bundle osx
# fi

# # Customized Sublime Text plugin (for version 3)
# source $HOME/.zsh/plugins/sublime/sublime.plugin.zsh

# # Load the theme.
# antigen-theme $HOME/.zsh/themes/prose-knosis.zsh-theme

# # Tell antigen that you're done.
# antigen-apply

# # completion
# autoload -U compinit
# compinit

############################################################
# interactive shell customizations
############################################################


# ZSH ######################################################

# enable zsh online help
unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

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


# rbenv ####################################################
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# nvm ######################################################
[[ -s /Users/matt/.nvm/nvm.sh ]] && . /Users/matt/.nvm/nvm.sh


# vagrant ##################################################
export VAGRANT_DEFAULT_PROVIDER=parallels


# prompt ###################################################
# vcprompt: https://github.com/djl/vcprompt

setopt prompt_subst
autoload -Uz colors && colors

# virtualenv
VIRTUAL_ENV_DISABLE_PROMPT="true"
function virtualenv_info {
  [ $VIRTUAL_ENV ] && echo '('%{$fg_bold[grey]%}`basename $VIRTUAL_ENV`%{$reset_color%}') '
}

function vcprompt_info {
  vcprompt --format-git "on λ %{$fg[blue]%}%b%{$reset_color%}%{$fg[green]%}%u%m%a%{$reset_color%}" \
           --format-hg  "on ☿ %{$fg[magenta]%}%b%{$reset_color%}%{$fg[green]%}%u%m%{$reset_color%}" \
           --format    "on %s %{$fg[magenta]%}%b%{$reset_color%}%{$fg[green]%}%u%m%{$reset_color%}"
}

function box_name {
  hostname -s
}

PROMPT='%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}$(box_name)%{$reset_color%} in %{$fg[green]%}${PWD/#$HOME/~}%{$reset_color%} $(vcprompt_info) $(virtualenv_info)
%(?,,%{${bg[red]}%}%{$fg[white]%}[%?]%{$reset_color%} )%# '

# local return_status="%{$fg[red]%}%(?..×)%{$reset_color%}"
# RPROMPT='${return_status}%{$reset_color%}'


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

# add color to grep
export GREP_OPTIONS='--color=auto'

# use vim as pager / less replacement
if [[ "$OSTYPE" == "darwin"* ]] || [[ "$OSTYPE" == "linux"* ]]; then
	export PAGER=vimpager
	alias less=$PAGER
	# use vim as an editor
	export EDITOR=vim
fi


# PATH #####################################################
export PATH="$HOME/bin:/usr/local/share/npm/bin:/usr/local/lib/node_modules:/usr/local/bin:/usr/local/sbin:$PATH"
