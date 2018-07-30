#! /usr/bin/env bash
# Runs at non-login interactive shell (except macOS)

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

export PS1="\[\033[48;5;${__prompt_cmdstatus_bg}m\033[38;5;${__prompt_cmdstatus_fg}m\]\$(s=\$? && [ \$s != 0 ] && echo "[\$s]")\[\033[48;5;${__prompt_ssh_username_bg}m\033[38;5;${__prompt_ssh_username_fg}m\] \u \[\033[48;5;${__prompt_hostname_bg}m\033[38;5;${__prompt_hostname_fg}m\] \h \[\033[48;5;${__prompt_left_directory_bg}m\033[38;5;${__prompt_left_directory_fg}m\] \w \[\033[48;5;${__prompt_bg}m\033[38;5;${__prompt_fg}m\]\n\$\[\033[0m\] "

source_if () {
  [ -e "$1" ] && . "$1"
}

# installed () {
#   command -v "$1" >/dev/null 2>&1
# }

source_if "$HOME/.env"
source_if "$HOME/.aliases"
source_if "$HOME/.bin/__prompt"

source_if "/usr/local/share/bash-completion/bash_completion"

# iTerm2
[ "$TERM_PROGRAM" = "iTerm.app" ] && \
  source_if "$HOME/.iterm2/iterm2_shell_integration.bash"

# `bash-completion` and `emacs-bash-completion`
# brew install bash-completion@2
if [[ ( -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ) &&\
        -f /etc/bash_completion ]]; then
  . /etc/bash_completion
fi

# direnv
installed direnv && eval "$(direnv hook bash)"

# rbenv
installed rbenv && eval "$(rbenv init -)"

# nvm
if [ -f "/usr/local/opt/nvm/nvm.sh" ]; then
  mkdir "$HOME/.nvm"
  export NVM_DIR="$HOME/.nvm"
  . "/usr/local/opt/nvm/nvm.sh"
fi
