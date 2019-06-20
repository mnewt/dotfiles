#! /usr/bin/env bash
# Runs at non-login interactive shell

# Construct a pleasant prompt that clearly delineates output and command entry
bold='\033[1m'
normal='\033[0m'
bg='\033[48;5;'
fg='\033[38;5;'

bg() {
  printf "${bg}%sm" "$1"
}

fg() {
  printf "${fg}%sm" "$1"
}

cmdstatus="\[$(bg 001)$(fg 255)\]"'$(s=$? && [ $s != 0 ] && echo " $s ")'
# Only display username and hostname if we are over SSH
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  username="\[$(bg 003)$(fg 000)\] \u "
  hostname="\[$(bg 002)$(fg 000)\] \h "
else
  username=""
  hostname=""
fi
dir="\[$(bg 006)$(fg 016)\] \w "
sigil="\[${normal}\]\n\[${bold}\]\$\[${normal}\] "

# Only display a fancy prompt if we are in an interactive, smart terminal.
case "$TERM" in
  xterm*|rxvt*|eterm*|screen*)
    tty -s && export PS1="${cmdstatus}${username}${hostname}${dir}${sigil}"
    ;;
  *)
    export PS1="$PWD> "
    ;;
esac

# Bind M-p and M-n to help with Emacs muscle memory.
if [[ ! -v INSIDE_EMACS ]]; then
  bind '"\ep":previous-history'
  bind '"\en":next-history'
fi

source_if () {
  [ -e "$1" ] && . "$1"
}

# installed () {
#   command -v "$1" >/dev/null 2>&1
# }

source_if "$HOME/.env"
source_if "$HOME/.aliases"
source_if "$HOME/.bin/__prompt"

# iTerm2
[ "$TERM_PROGRAM" = "iTerm.app" ] && \
  source_if "$HOME/.iterm2/iterm2_shell_integration.bash"

# `bash-completion` and `emacs-bash-completion`
# brew install bash-completion@2
if [[ -z "$INSIDE_EMACS" || "$EMACS_BASH_COMPLETE" = "t" ]]; then
  source_if "/usr/local/share/bash-completion/bash_completion"
  source_if "/usr/local/etc/bash_completion"
  source_if "/etc/bash_completion"
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
