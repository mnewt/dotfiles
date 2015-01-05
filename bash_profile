# ~/.bash_profile

# prompt
export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\] at \[\e[0m\]\[\e[00;32m\]\h\[\e[0m\]\[\e[00;37m\] in \[\e[0m\]\[\e[00;34m\]\w\[\e[0m\]\[\e[00;37m\] \\$\[\e[0m\]"

# load private environment variables
if [ -e "$HOME/.private" ]; then
  source "$HOME/.private"
fi

# aliases
if [ -e "$HOME/.aliases" ]; then
  source "$HOME/.aliases"
fi
