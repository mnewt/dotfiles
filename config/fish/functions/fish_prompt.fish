# name: mnewt
# inspired by: Numist, https://gist.github.com/douglascamata/e409fd77e9c61dcbdc44
# and: https://gist.github.com/gak/5747159
#
# requires: fish, ncurses
#
# to benchmark, use the command:
# time -p fish -c 'fish_prompt'

# Just calculate these once, to save a few cycles when displaying the prompt
if not set -q __fish_prompt_hostname
  set -g __fish_prompt_hostname (hostname -s)
end

if not set -q cyan
  # Colours
  set -g black (set_color black)
  set -g red (set_color red)
  set -g green (set_color green)
  set -g brown (set_color brown)
  set -g yellow (set_color yellow)
  set -g blue (set_color blue)
  set -g magenta (set_color magenta)
  set -g purple (set_color purple)
  set -g cyan (set_color cyan)
  set -g white (set_color white)
  set -g normal (set_color normal)

  # non standard colors
  set -g cyan (set_color 42f8ca)
  set -g gray (set_color 707070)

  # cyan gradient
  set -g c1 (set_color 059b75)
  set -g c2 (set_color 07cc9a)
  set -g c3 (set_color 10f7bc)
  set -g c4 (set_color 41f8ca)
  set -g c5 (set_color 6ff0cf)
  set -g c6 (set_color adf6e4)

  set -g ce (set_color $fish_color_error)
end

# if not set -q __fish_git_prompt_showstashstate
#   set -g __fish_git_prompt_showstashstate 1
#   set -g __fish_git_prompt_showuntrackedfiles 1
#   set -g __fish_git_prompt_showdirtystate 1
#   set -g __fish_git_prompt_showupstream 'auto'
#   set -g __fish_git_prompt_showcolorhints 1
#   # set -g __fish_git_prompt_color_branch (set_color purple)
# end

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


# Outputs first argument left-aligned, second argument right-aligned, newline
# function _rprint
#   if [ (count $argv) = 1 ]
#     echo -s $argv
#   else
#     set -l arglength (expr length + $argv[1]$argv[2])
#     set -l termwidth (tput cols)
#     set -l padding " "
#     if [ $arglength -lt $termwidth ]
#       set padding (printf "%"(expr $termwidth - $arglength)"s")
#     end

#     echo -n "$argv[1]$padding$argv[2]"
#   end
# end

function fish_prompt --description "Write out the prompt"
  # Last command
  set -l last_status $status

  # If in ssh session, print username and hostname
  if test -n "$SSH_CLIENT"
    echo -ns $yellow$USER$gray " at " $green$__fish_prompt_hostname$gray " in "
  end

  # Current Directory
  if test "$OSTYPE" = CYGWIN
    # shorten the path for CYGWIN, since we want the whole prompt on one line
    echo -ns $c1(prompt_pwd | sed "s,/,$c1/$c3,g" | sed "s,\(.*\)/[^m]*m,\1/$c4,")
  else
    # 1st sed replaces home dir with '~'
    # 2nd sed colorizes forward slashes
    # 3rd sed colorizes the deepest path (the 'm' is the last char in the
    # ANSI color code that needs to be stripped)
    echo -ns $c1(pwd | sed "s:^$HOME:~:" | sed "s,/,$c1/$c3,g" | sed "s,\(.*\)/[^m]*m,\1/$c4,")
  end

  # Git

  # This method is nice, easy, and detailed but slow (especially over SMB/NFS)
  #echo -ns (__fish_git_prompt "$gray on $purple%s")

  # bare bones, much faster git info
  # check if we're in a git repo
  if git rev-parse --is-inside-work-tree >/dev/null ^/dev/null
    # branch name
    set -l git_branch (git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
    echo -ns "$gray on $purple$git_branch"
    set -l git_dirty (git status --porcelain --ignore-submodules)
    if test -n "$git_dirty"
      # repo is dirty
      echo -ns (set_color -o purple) '*'
    end
  end


  # Ruby
  # set -l ruby_info
  # if which rvm-prompt >/dev/null ^&1
  #   set ruby_info (rvm-prompt i v g)
  # else
  #   if which rbenv >/dev/null ^&1
  #     set ruby_info (rbenv version-name)
  #   end
  # end
  # test $ruby_info; and set ruby_info "$gray""using $magenta‹$ruby_info›"


  # The Cygwin/mintty/fish combination doesn't handle multi-line prompts well
  if test "$OSTYPE" != 'CYGWIN'
    echo
  end

    # Print last command status if nonzero
  set -e status_info
  if test $last_status -ne 0
    echo -ns "$gray" (set_color -b $fish_color_error) "$last_status" (set_color -b normal)
  end

  # Prompt delimiter
  if test "$USER" = "root"
    echo -ns "$red# "
  else
    echo -ns "$white> "
  end

end