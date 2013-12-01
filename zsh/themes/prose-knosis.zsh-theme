#!/bin/sh

# vcprompt: https://github.com/djl/vcprompt

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
