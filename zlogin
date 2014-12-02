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
fpath=($HOME/.zsh/completions $fpath)

# load all executable scripts from plugins directory
for file in $(find $HOME/.zsh/plugins -maxdepth 1 -name "*.zsh"); do
  source $file
done

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
[[ -s /$HOME/.nvm/nvm.sh ]] && . /$HOME/.nvm/nvm.sh

# vagrant ##################################################
export VAGRANT_DEFAULT_PROVIDER=parallels


# prompt ###################################################


zstyle ':vcs_info:*' actionformats 'on %s %b running %a %u%c%R%r%m'
zstyle ':vcs_info:*' formats 'on %s %b %u%c%m %i'
precmd () { vcs_info }



# prompt style and colors based on Steve Losh's Prose theme:
# http://github.com/sjl/oh-my-zsh/blob/master/themes/prose.zsh-theme
#
# vcs_info modifications from Bart Trojanowski's zsh prompt:
# http://www.jukie.net/bart/blog/pimping-out-zsh-prompt
#
# git untracked files modification from Brian Carper:
# http://briancarper.net/blog/570/git-info-in-your-zsh-prompt
# 
# sliced up from 
# https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/steeef.zsh-theme

export VIRTUAL_ENV_DISABLE_PROMPT=1

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('$fg[blue]`basename $VIRTUAL_ENV`%{$reset_color%}') '
}
PR_GIT_UPDATE=1

setopt prompt_subst
autoload colors
colors

autoload -U add-zsh-hook
autoload -Uz vcs_info

#use extended color pallete if available
if [[ $TERM = *256color* || $TERM = *rxvt* ]]; then
    turquoise="%F{81}"
    orange="%F{166}"
    purple="%F{135}"
    hotpink="%F{161}"
    limegreen="%F{118}"
else
    turquoise="$fg[cyan]"
    orange="$fg[yellow]"
    purple="$fg[magenta]"
    hotpink="$fg[red]"
    limegreen="$fg[green]"
fi

# enabling only git detection - for all backends supported, run `vcs_info_printsys`
zstyle ':vcs_info:*' enable git

# check-for-changes can be really slow.
# you should disable it, if you work with large repositories
zstyle ':vcs_info:*:prompt:*' check-for-changes true

# set formats
# %b - branchname
# %u - unstagedstr (see below)
# %c - stagedstr (see below)
# %a - action (e.g. rebase-i)
# %R - repository path
# %S - path in the repository
PR_RST="%{${reset_color}%}"
FMT_BRANCH="on %{$turquoise%}%b%u%c${PR_RST}"
FMT_ACTION="on %{$limegreen%}%a${PR_RST}"
FMT_UNSTAGED="%{$yellow%}●"
FMT_STAGED="%{$limegreen%}+"

zstyle ':vcs_info:*:prompt:*' unstagedstr   "${FMT_UNSTAGED}"
zstyle ':vcs_info:*:prompt:*' stagedstr     "${FMT_STAGED}"
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}"
zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats   ""


function steeef_preexec {
    case "$(history $HISTCMD)" in
        *git*)
            PR_GIT_UPDATE=1
            ;;
        *svn*)
            PR_GIT_UPDATE=1
            ;;
    esac
}
add-zsh-hook preexec steeef_preexec

function steeef_chpwd {
    PR_GIT_UPDATE=1
}
add-zsh-hook chpwd steeef_chpwd

function steeef_precmd {
    if [[ -n "$PR_GIT_UPDATE" ]] ; then
        # check for untracked files or updated submodules, since vcs_info doesn't
        if git ls-files --other --exclude-standard 2> /dev/null | grep -q "."; then
            PR_GIT_UPDATE=1
            FMT_BRANCH="on %{$turquoise%}%b%u%c%{$hotpink%}=${PR_RST}"
        else
            FMT_BRANCH="on %{$turquoise%}%b%u%c${PR_RST}"
        fi
        zstyle ':vcs_info:*:prompt:*' formats "on %{$turquoise%}%b%u%c${PR_RST}"

        vcs_info 'prompt'
        PR_GIT_UPDATE=
    fi
}
add-zsh-hook precmd steeef_precmd


PROMPT='%{$purple%}%n%{$reset_color%} at %{$orange%}%m%{$reset_color%} in %{$limegreen%}${PWD/#$HOME/~}%{$reset_color%} ${vcs_info_msg_0_} $(virtualenv_info)
%(?,,%{${bg[red]}%}%{$fg[white]%}[%?]%{$reset_color%} )%# '


# Other Shell Customizations ###############################

# load private environment variables
if [ -e "$HOME/.private" ]; then
  source "$HOME/.private"
fi

# aliases
if [ -e "$HOME/.aliases" ]; then
  source "$HOME/.aliases"
fi

# use vim as pager / less replacement
if hash vimpager 2>/dev/null; then
	export PAGER=vimpager
	alias less=$PAGER
	# use vim as an editor
	export EDITOR=vim
fi


# PATH #####################################################
export PATH="$HOME/.bin:$HOME/bin:$PATH"
