#!/bin/bash

# simple prompt in case the fancy one isn't used
__prompt_cmdstatus_bg=001
__prompt_cmdstatus_fg=255
__prompt_cmdstatus_format=' %i '

__prompt_ssh_username_bg=003
__prompt_ssh_username_fg=000
__prompt_ssh_username_format=' %s '

__prompt_hostname_bg=002
__prompt_hostname_fg=000
__prompt_hostname_format=' %s '

__prompt_left_directory_bg=006
__prompt_left_directory_fg=016
__prompt_left_directory_format=' %s '

__prompt_bg=000
__prompt_fg=255

export PS1="\[\033[48;5;${__prompt_cmdstatus_bg}m\033[38;5;${__prompt_cmdstatus_fg}m\]\$(s=\$? && [ \$s != 0 ] && echo "[\$s]")\[\033[48;5;${__prompt_ssh_username_bg}m\033[38;5;${__prompt_ssh_username_fg}m\] \u \[\033[48;5;${__prompt_hostname_bg}m\033[38;5;${__prompt_hostname_fg}m\] \h \[\033[48;5;${__prompt_left_directory_bg}m\033[38;5;${__prompt_left_directory_fg}m\] \w \[\033[48;5;${__prompt_bg}m\033[38;5;${__prompt_fg}m\] \$\[\033[0m\] "

source_if () {
  [ -e "$1" ] && . "$1"
}

source_if "$HOME/.profile"
source_if "$HOME/.bashrc"
source_if "$HOME/.aliases"
source_if "$HOME/.private.sh"
source_if "$HOME/.bin/__prompt"

source_if "$HOME/.bin/start-ssh-agent"

[ "$TERM_PROGRAM" = "iTerm.app" ] && \
  source_if "$HOME/.iterm2/iterm2_shell_integration.bash"

installed direnv && eval "$(direnv hook bash)"

installed rbenv && eval "$(rbenv init -)"
