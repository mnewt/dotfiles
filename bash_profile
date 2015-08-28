# ~/.bash_profile

# simple prompt
# export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\] at \[\e[0m\]\[\e[00;32m\]\h\[\e[0m\]\[\e[00;37m\] in \[\e[0m\]\[\e[00;34m\]\w\[\e[0m\]\[\e[00;37m\] \\$\[\e[0m\] "

# aliases
if [ -e "$HOME/.aliases" ]; then
  source "$HOME/.aliases"
fi

# load private environment variables
# if [ -e "$HOME/.private" ]; then
#   source "$HOME/.private"
# fi


#
# PROMPT
#

# Cygwin is special
if [ ! -z ${OSTYPE} ]; then
 case $(uname) in
 	"CYGWIN*")
		export OSTYPE='CYGWIN'
		;;
	'Darwin')
		export OSTYPE='Darwin'
		;;
	'Linux')
		export OSTYPE='Linux'
		;;
	esac
fi

# Just calculate these once, to save a few cycles when displaying the prompt

if [ "$OSTYPE" = "CYGWIN" ]; then
	export __bash_prompt_hostname=$(/bin/hostname)
else
	export __bash_prompt_hostname=$(hostname -s)
fi



# Standard Colors
black='\e[0;30m'
red='\e[0;31m'
green='\e[0;32m'
yellow='\e[0;33m'
blue='\e[0;34m'
purple='\e[0;35m'
cyan='\e[0;36m'
white='\e[0;37m'

# Non Standard Colors
gray='\x1b[38;5;232m'

# cyan gradient
c1="$cyan"
c2="$cyan"
c3="$cyan"
c4="$cyan"
c5="$cyan"
c6="$cyan"

ce='\x1b[38;5;165m'

normal='\e[0;39m'

function bash_prompt {
	last_status=$?

	if [ -n "$SSH_CLIENT" ]; then
		printf "$yellow$USER$gray at $green$__bash_prompt_hostname$gray in "
	fi

  # 1st expression replaces home dir with '~'
  # 2nd expression colorizes forward slashes
  # 3rd expression colorizes the deepest path (the 'm' is the last char in the
  # ANSI color code that needs to be stripped)
  printf ${c1}$(pwd | sed -e "s:^${HOME}:~:" -e "s,/,/,g")

	if git rev-parse --is-inside-work-tree 2>/dev/null >/dev/null; then
		# branch name
		git_branch=$(git symbolic-ref HEAD 2>/dev/null | sed 's|^refs/heads/||')
		printf "$gray git $purple$git_branch"
		dirty=$(git status --porcelain --ignore-submodules 2>/dev/null)
		if [ -n "$dirty" ]; then
      # repo is dirty
      printf "$purple*"
   	fi
  fi

  # Python / virtualenv
  if [ -n "$VIRTUAL_ENV" ]; then
    printf "$gray venv $blue" $(basename $VIRTUAL_ENV)
  fi

  # The Cygwin/mintty/fish combination doesn't handle multi-line prompts well
  if [ "$OSTYPE" != 'CYGWIN' ]; then
    printf '\n'
  fi

  # Print last command status if nonzero
  if [ "$last_status" -ne 0 ]; then
    printf "$gray$ce$last_status"
  fi

  # Prompt delimiter
  if [ "$USER" = "root" ]; then
    export PS1="$red#$normal "
  else
    export PS1="$white\$$normal "
  fi
}

export PROMPT_COMMAND=bash_prompt