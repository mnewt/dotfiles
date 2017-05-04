# Status line style fish prompt
#

# Just calculate this once to reduce calls to external programs
if not set -q __fish_prompt_hostname
  set -g __fish_prompt_hostname (hostname -s)
end

function upsearch
  set directory "$PWD"
  while test -n "$directory"
    if test -e "$directory/$argv[1]"
      echo -n -s "$directory/$argv[1]"
      return 0
    end
    set directory (string replace -r '\/[^\/]*$' '' "$directory")
  end
  return 1
end

function directory_helper
  echo -n -s (string replace "$HOME" "~" "$PWD")
end

function git_helper
  # detect .git directory
  # print current branch; add an asterisk if there are uncommitted changes
  set -l gitdir (string replace "/.git" "" (upsearch .git)); or return 1
  echo -n -s (cat "$gitdir/.git/HEAD" | string match -r '[^\/]*$')
  count (git -C "$gitdir" status --porcelain) >/dev/null
    and echo -n -s '*'
  return 0
end

function rbenv_helper
  set -q RBENV_SHELL; or return 1
  set -l rbenv_name (rbenv version-name)
  test $rbenv_name = "system"; and return 1
  printf $rbenv_name
end

function virtualenv_helper
  set -q VIRTUAL_ENV; or return 1
  echo -n -s (basename $VIRTUAL_ENV)
end

function node_helper
  # detect package.json
  # print "name" field
  set package (upsearch package.json); or return 1
  set -l name (cat "$package" | string match -r '(?<="name":\s")[\w\d]+')
  echo -n -s "$name[1]"
end

function vagrant_helper
  # detect Vagrantfile
  # print .vagrant/machines/$machine_name
  set vagrantfile (upsearch Vagrantfile); or return 1
  command ls 2>/dev/null (string replace -r '\/[^\/]*$' "/.vagrant/machines/" $vagrantfile)
   or echo -n -s "*"
end

function clojure_helper
  # detect project.clj or build.boot
  # print ...
  if set bootfile (upsearch build.boot)
    cat "$bootfile" | string match -r "(?<=:project\s')[^\s]+"
    return 0
  else if set projectfile (upsearch project.clj)
    cat "$projectfile" | string match -r "(?<=\(defproject\s)[^\s]+"
    return 0
  else
    return 1
  end
end

function jobs_helper
  set -l jobs (jobs -c); or return 1
  set -l job_count (count $jobs)
  echo -n -s $job_count " job"
  test $job_count -gt 1; and echo -ns "s" # make jobs plural
  return 0
end

function tmux_helper
  set -q TMUX; and return 1
  set -l sessions (tmux ls 2>/dev/null); or return 1
  set -l session_count (echo "$sessions" | awk 'END{print NR}')
  test $session_count -gt 0; and echo -n -s "tmux: " $session_count
end

function datetime_helper
  date +"%Y-%m-%d %r"
end

# Lifted from (https://github.com/fish-shell/fish-shell/issues/1326)
function append --no-scope-shadowing
  if test (count $argv) -ne 2
    echo append: Expected 2 arguments, (count $argv) received.
    return 1
  end
  set -l __fish_value $$argv[1]
  set $argv[1] "$__fish_value$argv[2]"
end

# Evaluate the expression. If it returns without error then append the result
# of the expression to the variable, formatted according to the printf string
# Adapted from (https://github.com/fish-shell/fish-shell/issues/1326)
# argv[1]: variable to append to
# argv[2]: expression to evaluate
# argv[3]: printf string
# if_append variable expression "%s"
function if_append --no-scope-shadowing
  set -l result (eval "$argv[2]"); or return
  append "$argv[1]" "$__fish_value"(printf "$argv[3]" $result)
end

function with-color
  echo -n -s (set_color -b $argv[1]) (set_color $argv[2]) $argv[3] (set_color normal)
end

function do_str
  for i in (seq $argv[2])
    echo -n -s $argv[1]
  end
end

function fish_prompt -d 'Write out a prompt'
  set -l last_status $status

  set -l left
  if_append left "test $last_status -ne 0" (with-color B03760 FFFFFF " $last_status ")
  # Only display username and hostname if this is an ssh session
  if set -q SSH_CLIENT
    append left (with-color yellow black " $USER ")
    append left (with-color green black " $__fish_prompt_hostname ")
  end
  if_append left git_helper (with-color magenta 000 " %s ")
  if_append left rbenv_helper (with-color F1855E FFF " %s ")
  if_append left virtualenv_helper (with-color 055EFB FFF " %s ")
  if_append left node_helper (with-color 1C5C82 FFF " %s ")
  if_append left clojure_helper (with-color 6137AF FFF " %s ")
  if_append left vagrant_helper (with-color D6F52A 000 " %s ")
  if_append left directory_helper (with-color cyan 000 " %s ")

  set -l right
  if_append right jobs_helper (with-color A0A0A0 black " %s ")
  if_append right tmux_helper (with-color D0D0D0 black " %s ")
  if_append right datetime_helper (with-color 606060 DDD " %s ")

  # Strip out invisible characters before counting to get actual display width
  set -l raw_line (string replace -ra "(\e[^m]+m)|(\017)" "" "$left$right")
  set -l length (string length "$raw_line")
  set -l padding (math $COLUMNS - $length)
  test $padding -lt 0; and set right ""; and set padding 1
  echo -s \r "$left" (set_color -b 333) (do_str " " $padding) "$right"
  if test "$TERM_PROGRAM" = "iTerm.app"
    iterm2_prompt_mark
  end
  echo -n -s (set_color -o) "> " (set_color normal)
end
