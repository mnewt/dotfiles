# name: mnewt
# inspired by: Numist, https://gist.github.com/douglascamata/e409fd77e9c61dcbdc44
# and: https://gist.github.com/gak/5747159
#
# requires: fish, ncurses
#
# to do a half baked benchmark, use the command:
# time -p fish -c 'fish_prompt'

# Cygwin is special
if not set -q OSTYPE
  switch (uname)
    case CYGWIN'*'
      set OSTYPE 'CYGWIN'
    case 'Darwin'
      set OSTYPE 'Darwin'
    case 'Linux'
      set OSTYPE 'Linux'
  end
end

# Just calculate this once, to reduce calls to external programs
if not set -q __fish_prompt_hostname
  if test "$OSTYPE" = CYGWIN
    # I do not understand why this is necessary, but it is
    set -g __fish_prompt_hostname (/bin/hostname)
  else
    set -g __fish_prompt_hostname (hostname -s)
  end
end

function upsearch
  if test "$argv[1]" = "-q"
    set quiet "1"
    set thing "$argv[2]"
  else
    set quiet ""
    set thing "$argv[1]"
  end
  set directory "$PWD"
  while test -n "$directory"
    if test -e "$directory/$thing"
      if test -z "$quiet"
        echo -n -s "$directory/$thing"
      end
      return
    end
    set directory (string replace -r '\/[^\/]*$' '' "$directory")
  end
end

function username_helper
  if test -n "$SSH_CLIENT"
    echo -n -s "$__fish_prompt_hostname"
  end
end

function hostname_helper
  if test -n "$SSH_CLIENT"
    echo -n -s "$USER"
  end
end

function directory_helper
  if test "$OSTYPE" = CYGWIN
    # shorten the path for CYGWIN, since we want the whole prompt on one line
    echo -n -s (prompt_pwd)
  else
    echo -n -s (pwd | sed -e "s:^$HOME:~:")
  end
end

function git_helper
  # This method is nice, easy, and detailed but slow (especially over SMB/NFS)
  #echo -n -s (__fish_git_prompt "$gray on $purple%s")

  # bare bones, much faster git info
  # check if we're in a git repo
  if git rev-parse --is-inside-work-tree ^/dev/null >/dev/null
    # branch name
    set -l git_branch (git symbolic-ref HEAD ^/dev/null | sed 's|^refs/heads/||')
    echo -n -s "$git_branch"
    set -l dirty (git status --porcelain --ignore-submodules ^/dev/null)
    if test -n "$dirty"
      # repo is dirty
      echo -n -s '*'
    end
  end
end

function virtualenv_helper
  if set -q VIRTUAL_ENV
    echo -n -s (basename $VIRTUAL_ENV)
  end
end

function node_helper
  # detect package.json
  # report "name" field
  set package (upsearch package.json)
  if test -n "$package"
    cat "$package" | string match -r '(?<="name":\s")[\w\d]+'
  end
end

function vagrant_helper
  # detect Vagrantfile
  # report .vagrant/machines/$machine_name
  set vagrantfile (upsearch Vagrantfile)
  if test -n "$vagrantfile"
    command ls (string replace -r '\/[^\/]*$' '' $vagrantfile)"/.vagrant/machines/" | cut -d '/' -f 1
  end
end

function jobs_helper
  set -l job_count (jobs -c | wc -l | awk '{ print $1; }')
  if [ $job_count -gt 0 ]
    echo -ns $job_count " job"
    if [ $job_count -gt 1 ]
      # make jobs plural
      echo -ns "s"
    end
  end
end

function tmux_helper
  if test -z "$TMUX"
    set -l session_count (tmux ls 2>/dev/null | wc -l | awk '{ print $1; }')
    if [ $session_count -gt 0 ]
      echo -n -s "tmux: " $session_count
    end
  end
end

function time_helper
  date +"%Y-%m-%d %r"
end

# Adapted from (https://github.com/fish-shell/fish-shell/issues/1326)
function if_nonzero_append --no-scope-shadowing
  if test (count $argv) -ne 3
    echo if_nonzero_append: Expected 3 arguments, received (count $argv)
    return 1
  end
  if test -n "$argv[2]"
    set -l __fish_value $$argv[1]
    set $argv[1] "$__fish_value$argv[3]"
    return 0
  else
    return 1
  end
end

function plus_equals --no-scope-shadowing
  set -l __fish_value $$argv[1]
  set $argv[1] (math $__fish_value + $argv[2])
end

function with_color
  echo -n -s (set_color -b $argv[1]) (set_color $argv[2]) $argv[3] (set_color normal)
end

function fish_prompt -d 'Write out a prompt'
  set -l last_status $status
  set -l user (username_helper)
  set -l host (hostname_helper)
  set -l directory (directory_helper)
  set -l git_status (git_helper)
  set -l venv_status (virtualenv_helper)
  set -l node_status (node_helper)
  set -l vagrant_status (vagrant_helper)
  set -l jobs_status (jobs_helper)
  set -l tmux_status (tmux_helper)
  set -l datetime (time_helper)

  set length 0

  set -l left
  test "$last_status" -ne 0
    and set left "$left"(with_color red white "[$last_status]")
  if_nonzero_append left "$user" (with_color yellow black " $user ")
  if_nonzero_append left "$host" (with_color green black " $host ")
  if_nonzero_append left "$git_status" (with_color purple black " $git_status ")
  if_nonzero_append left "$venv_status" (with_color blue black " $venv_status ")
  if_nonzero_append left "$node_status" (with_color blue black " $node_status ")
  if_nonzero_append left "$vagrant_status" (with_color CC6688 black " $vagrant_status ")
  if_nonzero_append left "$directory" (with_color cyan black " $directory ")

  set -l right
  if_nonzero_append right "$jobs_status" (with_color A0A0A0 black " $jobs_status ")
  if_nonzero_append right "$tmux_status" (with_color D0D0D0 black " $tmux_status ")
  if_nonzero_append right "$datetime" (with_color 606060 DDDDDD " $datetime ")

  set -l term_width (tput cols)
  # echo left: "$left"
  # echo right: "$right"
  set -l length (string length (string replace -ra "\e[^m]+m" "" "$left$right"))
  # echo length: $length
  set -l padding_width (math $term_width - $length)
  if test $padding_width -lt 0
    set right ""
  end
  printf "$left"(set_color -b 333333)"%"$padding_width"s$right\n"(set_color -o)"> "(set_color normal)
end
